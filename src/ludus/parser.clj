(ns ludus.parser
  (:require
    [ludus.token :as token]
    [ludus.scanner :as scanner]
    [ludus.ast :as ast]
    [clojure.pprint :as pp]
    [clojure.set :as s]))

;; a parser map and some functions to work with them
(defn- parser [tokens]
  {::tokens tokens ::token 0 ::ast {} ::errors []})

(defn- current [parser]
  (nth (::tokens parser) (::token parser) nil))

(defn- ppeek [parser]
  (nth (::tokens parser) (inc (::token parser)) nil))

(defn- at-end? [parser]
  (let [curr (current parser)]
    (or (nil? curr) (= ::token/eof (::token/type curr)))))

(defn- advance [parser]
  (update parser ::token inc))

(defn- token-type [parser]
  (::token/type (current parser)))

(defn- node-type [parser]
  (get-in parser [::ast ::ast/type]))

;; some forward declarations
(declare parse-expr parse-word parse-pattern)

;; handle some errors
(def sync-on #{::token/newline
               ::token/semicolon
               ::token/comma
               ::token/rparen
               ::token/rbracket
               ::token/rbrace
               ::token/eof})

(defn- psync [parser message origin end]
  (let [poison {::ast/type ::ast/poison
                :message message
                :origin origin
                :end end}]
    (-> parser
      (assoc ::ast poison)
      (update ::errors conj poison))))

(defn- poisoned? [parser]
  (= ::ast/poison (get-in parser [::ast ::ast/type])))

(defn- panic
  ([parser message] (panic parser message sync-on))
  ([parser message sync-on]
   (println (str "PANIC!!! in the parser: " message))
   (let [sync-on (conj (if (set? sync-on) sync-on #{sync-on}) ::token/eof)
         origin (current parser)]
     (loop [parser parser]
       (let [curr (current parser)
             type (::token/type curr)]
         (if (or (at-end? parser) (contains? sync-on type))
           (psync parser message origin curr)
           (recur (advance parser))))))))

;; some helper functions
(defn- expect [tokens message parser]
  (let [curr (current parser)
        tokens (if (set? tokens) tokens #{tokens})
        type (::token/type curr)]
    (if (contains? tokens type)
      (advance parser)
      (-> parser
        (advance)
        (panic message tokens)))))

(defn- expect* [tokens message parser]
  (let [curr (current parser)
        tokens (if (set? tokens) tokens #{tokens})
        type (::token/type curr)]
    (if (contains? tokens type)
      {:success true :parser (advance parser)}
      {:success false :parser (panic (advance parser) message)})))

(defn- accept [tokens parser]
  (let [curr (current parser)
        tokens (if (set? tokens) tokens #{tokens})
        type (::token/type curr)]
    (if (contains? tokens type)
      (advance parser)
      parser)))

(defn- accept-many [tokens parser]
  (let [tokens (if (set? tokens) tokens #{tokens})]
    (loop [parser parser]
      (let [curr (current parser)
            type (::token/type curr)]
        (if (contains? tokens type)
          (recur (advance parser))
          parser)))))

;; various parsing functions
(defn- parse-atom [parser]
  (let [token (current parser)]
    (-> parser
      (advance)
      (assoc ::ast {::ast/type ::ast/atom
                    :token token
                    :value (::token/literal token)}))))

;; just a quick and dirty map to associate atomic words with values
(def atomic-words {::token/nil nil
                   ::token/true true
                   ::token/false false})

(defn parse-atomic-word [parser]
  (let [token (current parser)]
    (-> parser
      (advance)
      (assoc ::ast {::ast/type ::ast/atom
                    :token token
                    :value (get atomic-words (::token/type token))}))))

(defn- add-member [members member]
  (if (nil? member)
    members
    (conj members member)))

(defn- contains-placeholder? [members]
  (< 0 (count (filter #(= ::ast/placeholder (::ast/type %1)) members))))

(defn- parse-fn-tuple [origin]
  (loop [parser (accept-many #{::token/newline ::token/comma} (advance origin))
         members []
         current_member nil]
    (let [curr (current parser)]
      (case (token-type parser)
        ::token/rparen (let [ms (add-member members current_member)]
                         (assoc (advance parser) ::ast
                           {::ast/type ::ast/tuple
                            :length (count ms)
                            :members ms
                            :token (current origin)
                            :partial (contains-placeholder? ms)}))

        (::token/comma ::token/newline)
        (recur
          (accept-many #{::token/comma ::token/newline} parser)
          (add-member members current_member) nil)

        (::token/rbrace ::token/rbracket)
        (panic parser (str "Mismatched enclosure in tuple: " (::token/lexeme curr)))

        ::token/placeholder
        (if (contains-placeholder? members)
          (recur 
            (advance parser)
            members
            (panic parser "Partially applied functions must be unary. (Only one placeholder allowed in partial application.)" curr))
          (recur
            (advance parser) members {::ast/type ::ast/placeholder :token curr}))

        ::token/eof
        (panic (assoc origin ::errors (::errors parser)) "Unterminated tuple" ::token/eof)

        (let [parsed (parse-expr parser #{::token/comma ::token/newline ::token/rparen})]
          (recur parsed members (::ast parsed)))))))

(defn- parse-tuple [origin]
  (loop [parser (accept-many #{::token/newline ::token/comma} (advance origin))
         members []
         current_member nil]
    (let [curr (current parser)]
      (case (token-type parser)
        ::token/rparen (let [ms (add-member members current_member)]
                         (assoc (advance parser) ::ast
                           {::ast/type ::ast/tuple
                            :token (current origin)
                            :length (count ms)
                            :members ms}))

        (::token/comma ::token/newline)
        (recur
          (accept-many #{::token/comma ::token/newline} parser)
          (add-member members current_member) nil)

        (::token/rbrace ::token/rbracket)
        (panic parser (str "Mismatched enclosure in tuple: " (::token/lexeme curr)))

        ::token/placeholder
        (recur 
          (advance parser)
          members
          (panic parser "Placeholders in tuples may only be in function calls." curr))

        ::token/eof
        (panic (assoc origin ::errors (::errors parser)) "Unterminated tuple" ::token/eof)

        (let [parsed (parse-expr parser #{::token/comma ::token/newline ::token/rparen})]
          (recur parsed members (::ast parsed)))))))

(defn- parse-list [origin]
  (loop [parser (accept-many #{::token/newline ::token/comma} (advance origin))
         members []
         current_member nil]
    (let [curr (current parser)]
      (case (token-type parser)
        ::token/rbracket (let [ms (add-member members current_member)]
                           (assoc (advance parser) ::ast
                             {::ast/type ::ast/list
                              :token (current origin)
                              :members ms}))

        (::token/comma ::token/newline)
        (recur
          (accept-many #{::token/comma ::token/newline} parser)
          (add-member members current_member) nil)

        (::token/rbrace ::token/rparen)
        (panic parser (str "Mismatched enclosure in list: " (::token/lexeme curr)))

        ::token/eof
        (panic (assoc origin ::errors (::errors parser)) "Unterminated list" ::token/eof)

        (let [parsed (parse-expr parser #{::token/comma ::token/newline ::token/rbracket})]
          (recur parsed members (::ast parsed)))))))

(defn- parse-set [origin]
  (loop [parser (accept-many #{::token/newline ::token/comma} (advance origin))
         members []
         current_member nil]
    (let [curr (current parser)]
      (case (token-type parser)
        ::token/rbrace (let [ms (add-member members current_member)]
                         (assoc (advance parser) ::ast
                           {::ast/type ::ast/set
                            :token (current origin)
                            :members ms}))

        (::token/comma ::token/newline)
        (recur
          (accept-many #{::token/comma ::token/newline} parser)
          (add-member members current_member) nil)

        (::token/rbracket ::token/rparen)
        (panic parser (str "Mismatched enclosure in set: " (::token/lexeme curr)))

        ::token/eof
        (panic (assoc origin ::errors (::errors parser)) "Unterminated set" ::token/eof)

        (let [parsed (parse-expr parser #{::token/comma ::token/newline ::token/rbrace})]
          (recur parsed members (::ast parsed)))))))

(defn- parse-hash [origin]
  (loop [parser (accept-many #{::token/newline ::token/comma} (advance origin))
         members {}
         current_member nil]
    (let [curr (current parser)]
      (case (token-type parser)
        ::token/rbrace (let [ms (add-member members current_member)]
                         (assoc (advance parser) ::ast
                           {::ast/type ::ast/hash
                            :token (current origin)
                            :members ms}))

        (::token/comma ::token/newline)
        (recur
          (accept-many #{::token/comma ::token/newline} parser)
          (add-member members current_member) nil)

        (::token/rbracket ::token/rparen)
        (panic parser (str "Mismatched enclosure in hashmap: " (::token/lexeme curr)))

        ::token/eof
        (panic (assoc origin ::errors (::errors parser)) "Unterminated hashmap" ::token/eof)

        ::token/word
        (if (not current_member) (let [parsed (parse-word parser) word (get-in parsed [::ast :word])]
                                   (recur parsed members {(keyword word) (::ast parsed)}))
          (panic parser "Hashmap entries must be single words or keyword+expression pairs." #{::token/rbrace}))

        ::token/keyword
        (if (not current_member) (let [kw (parse-atom parser) expr (parse-expr kw #{::token/comma ::token/newline ::token/rbrace})]
                                   (recur expr members {(:value (::ast kw)) (::ast expr)}))
          (panic parser "Hashmap entries must be single words or keyword+expression pairs." #{::token/rbrace}))

        (panic parser "Hashmap entries must be single words or keyword+expression pairs" #{::token/rbrace})))))

(defn- parse-struct [origin]
  (loop [parser (accept-many #{::token/newline ::token/comma} (advance origin))
         members {}
         current_member nil]
    (let [curr (current parser)]
      (case (token-type parser)
        ::token/rbrace (let [ms (add-member members current_member)]
                         (assoc (advance parser) ::ast
                           {::ast/type ::ast/struct
                            :token (current origin)
                            :members ms}))

        (::token/comma ::token/newline)
        (recur
          (accept-many #{::token/comma ::token/newline} parser)
          (add-member members current_member) nil)

        (::token/rbracket ::token/rparen)
        (panic parser (str "Mismatched enclosure in struct: " (::token/lexeme curr)))

        ::token/eof
        (panic (assoc origin ::errors (::errors parser)) "Unterminated struct" ::token/eof)

        ::token/word
        (if (not current_member) (let [parsed (parse-word parser) word (get-in parsed [::ast :word])]
                                   (recur parsed members {(keyword word) (::ast parsed)}))
          (panic parser "Struct entries must be single words or keyword+expression pairs." #{::token/rbrace}))

        ::token/keyword
        (if (not current_member) (let [kw (parse-atom parser) expr (parse-expr kw #{::token/comma ::token/newline ::token/rbrace})]
                                   (recur expr members {(:value (::ast kw)) (::ast expr)}))
          (panic parser "Struct entries must be single words or keyword+expression pairs." #{::token/rbrace}))

        (panic parser "Struct entries must be single words or keyword+expression pairs" #{::token/rbrace})))))

(defn- parse-ns [ns-root]
  (let [name (expect* #{::token/word} "Expected ns name" (advance ns-root))
        origin (expect* #{::token/lbrace} "Expected { after ns name" (:parser name))]
    (cond 
      (not (:success name)) (panic parser "Expected ns name" #{::token/newline})

      (not (:success origin)) (panic (:parser name) "Expected { after ns name")

      :else 
      (loop [parser (accept-many #{::token/newline ::token/comma} (:parser origin))
             members {}
             current_member nil]
        (let [curr (current parser)]
          (case (token-type parser)
            ::token/rbrace (let [ms (add-member members current_member)]
                             (assoc (advance parser) ::ast
                               {::ast/type ::ast/ns
                                :token (current ns-root)
                                :name (get-in (parse-word (advance ns-root)) [::ast :word])
                                :members ms}))

            (::token/comma ::token/newline)
            (recur
              (accept-many #{::token/comma ::token/newline} parser)
              (add-member members current_member) nil)

            (::token/rbracket ::token/rparen)
            (panic parser (str "Mismatched enclosure in ns: " (::token/lexeme curr)))

            ::token/eof
            (panic (assoc origin ::errors (::errors parser)) "Unterminated ns" ::token/eof)

            ::token/word
            (if (not current_member) (let [parsed (parse-word parser) word (get-in parsed [::ast :word])]
                                       (recur parsed members {(keyword word) (::ast parsed)}))
              (panic parser "ns entries must be single words or keyword+expression pairs." #{::token/rbrace}))

            ::token/keyword
            (if (not current_member) (let [kw (parse-atom parser) expr (parse-expr kw #{::token/comma ::token/newline ::token/rbrace})]
                                       (recur expr members {(:value (::ast kw)) (::ast expr)}))
              (panic parser "ns entries must be single words or keyword+expression pairs." #{::token/rbrace}))

            (panic parser "ns entries must be single words or keyword+expression pairs" #{::token/rbrace})))))))

(defn- parse-block [origin]
  (loop [parser (accept-many #{::token/newline ::token/semicolon} (advance origin))
         exprs []
         current_expr nil]
    (let [curr (current parser)]
      (case (token-type parser)
        ::token/rbrace
        (let [es (add-member exprs current_expr)]
          (if (empty? es)
            (advance (panic parser "Blocks must have at least one expression"))
            (assoc (advance parser) ::ast {::ast/type ::ast/block
                                           :token (current origin)
                                           :exprs es})))

        (::token/semicolon ::token/newline)
        (recur
          (accept-many #{::token/newline ::token/semicolon} parser)
          (add-member exprs current_expr) nil)

        (::token/rbracket ::token/rparen)
        (panic parser (str "Mismatched enclosure in block: " (::token/lexeme curr)))

        ::token/eof
        (panic (assoc origin ::errors (::errors parser)) "Unterminated block" ::token/eof)

        (let [parsed
              (if current_expr
                (panic parser "Expected end of expression" #{::token/semicolon ::token/newline})
                (parse-expr parser))]
          (recur parsed exprs (::ast parsed)))))))

(defn parse-script [origin]
  (loop [parser (accept-many #{::token/newline ::token/semicolon} origin)
         exprs []
         current_expr nil]
    (case (token-type parser)
      ::token/eof
      (let [es (add-member exprs current_expr)]
        (if (empty? es)
          (panic parser "Scripts must have at least one expression")
          (assoc parser ::ast {::ast/type ::ast/script 
                               :token (current origin) :exprs es})))

      (::token/semicolon ::token/newline)
      (recur
        (accept-many #{::token/semicolon ::token/newline} parser)
        (add-member exprs current_expr)
        nil)

      (let [parsed
            (if current_expr
              (panic parser "Expected end of expression" #{::token/semicolon ::token/newline})
              (parse-expr parser))]

        (recur parsed exprs (::ast parsed))))))

(defn- parse-synthetic [parser]
  (loop [parser parser
         terms []]
    (let [curr (current parser)
          type (::token/type curr)]
      (case type
        ::token/keyword
        (recur (advance parser) (conj terms (::ast (parse-atom parser))))

        ::token/word
        (recur (advance parser) (conj terms (::ast (parse-word parser))))

        ::token/lparen
        (let [parsed (parse-fn-tuple parser)]
          (recur parsed (conj terms (::ast parsed))))

        (assoc parser ::ast {::ast/type ::ast/synthetic :token (current parser) :terms terms})))))

(defn- parse-word [parser]
  (let [curr (current parser)]
    (-> parser
      (advance)
      (assoc ::ast {::ast/type ::ast/word :token (current parser) :word (::token/lexeme curr)}))))

(def sync-pattern (s/union sync-on #{::token/equals ::token/rarrow}))

(defn- parse-list-pattern [origin]
  (loop [parser (accept-many #{::token/newline ::token/comma} (advance origin))
         members []
         current_member nil]
    (let [curr (current parser)]
      (case (token-type parser)
        ::token/rbracket (let [ms (add-member members current_member)]
                           (assoc (advance parser) ::ast
                             {::ast/type ::ast/list
                              :token (current origin)
                              :members ms}))

        (::token/comma ::token/newline)
        (recur
          (accept-many #{::token/comma ::token/newline} parser)
          (add-member members current_member) nil)

        (::token/rbrace ::token/rparen)
        (panic parser (str "Mismatched enclosure in list pattern: " (::token/lexeme curr)))

        ::token/eof
        (panic (assoc origin ::errors (::errors parser)) "Unterminated list pattern" ::token/eof)

        (let [parsed (parse-pattern parser)]
          (recur parsed members (::ast parsed)))))))

(defn- parse-hash-pattern [origin]
  (loop [parser (accept-many #{::token/newline ::token/comma} (advance origin))
         members {}
         current_member nil]
    (let [curr (current parser)]
      (case (token-type parser)
        ::token/rbrace (let [ms (add-member members current_member)]
                         (assoc (advance parser) ::ast
                           {::ast/type ::ast/hash
                            :token (current origin)
                            :members ms}))

        (::token/comma ::token/newline)
        (recur
          (accept-many #{::token/comma ::token/newline} parser)
          (add-member members current_member) nil)

        (::token/rbracket ::token/rparen)
        (panic parser (str "Mismatched enclosure in hashmap pattern: " (::token/lexeme curr)))

        ::token/eof
        (panic (assoc origin ::errors (::errors parser)) "Unterminated hashmap pattern" ::token/eof)

        ::token/word
        (if (not current_member) 
          (let [parsed (parse-word parser) word (get-in parsed [::ast :word])]
            (recur parsed members {(keyword word) (::ast parsed)}))
          (panic parser "Hashmap patterns may only include single words or keyword+pattern pairs." #{::token/rbrace}))

        ::token/keyword
        (if (not current_member) 
          (let [kw (parse-atom parser) pattern (parse-pattern kw)]
            (recur pattern members {(:value (::ast kw)) (::ast pattern)}))
          (panic parser "Hashmap patterns may only include single words or keyword+pattern pairs." #{::token/rbrace}))

        (panic parser "Hashmap patterns may only include single words or keyword+pattern pairs" #{::token/rbrace})))))

(defn- parse-struct-pattern [origin]
  (loop [parser (accept-many #{::token/newline ::token/comma} (advance origin))
         members {}
         current_member nil]
    (let [curr (current parser)]
      (case (token-type parser)
        ::token/rbrace (let [ms (add-member members current_member)]
                         (assoc (advance parser) ::ast
                           {::ast/type ::ast/struct
                            :token (current origin)
                            :members ms}))

        (::token/comma ::token/newline)
        (recur
          (accept-many #{::token/comma ::token/newline} parser)
          (add-member members current_member) nil)

        (::token/rbracket ::token/rparen)
        (panic parser (str "Mismatched enclosure in struct pattern: " (::token/lexeme curr)))

        ::token/eof
        (panic (assoc origin ::errors (::errors parser)) "Unterminated struct pattern" ::token/eof)

        ::token/word
        (if (not current_member) 
          (let [parsed (parse-word parser) word (get-in parsed [::ast :word])]
            (recur parsed members {(keyword word) (::ast parsed)}))
          (panic parser "Struct patterns may only include single words or keyword+pattern pairs." #{::token/rbrace}))

        ::token/keyword
        (if (not current_member) 
          (let [kw (parse-atom parser) pattern (parse-pattern kw)]
            (recur pattern members {(:value (::ast kw)) (::ast pattern)}))
          (panic parser "Struct patterns may only include single words or keyword+pattern pairs." #{::token/rbrace}))

        (panic parser "Struct patterns may only include single words or keyword+pattern pairs." #{::token/rbrace})))))

(defn- parse-tuple-pattern [origin]
  (loop [parser (accept-many #{::token/newline ::token/comma} (advance origin))
         members []
         current_member nil]
    (let [curr (current parser)]
      (case (token-type parser)
        ::token/rparen (let [ms (add-member members current_member)]
                         (assoc (advance parser) ::ast
                           {::ast/type ::ast/tuple
                            :token (current origin)
                            :length (count ms)
                            :members ms}))

        (::token/comma ::token/newline)
        (recur
          (accept-many #{::token/comma ::token/newline} parser)
          (add-member members current_member) nil)

        (::token/rbrace ::token/rbracket)
        (panic parser (str "Mismatched enclosure in tuple: " (::token/lexeme curr)))

        ::token/eof
        (panic (assoc origin ::errors (::errors parser)) "Unterminated tuple" ::token/eof)

        (let [parsed (parse-pattern parser)]
          (recur parsed members (::ast parsed)))))))

(defn- parse-pattern [parser]
  (let [curr (current parser)
        type (::token/type curr)]
    (case type
      (::token/placeholder ::token/ignored) 
      (-> parser
        (advance)
        (assoc ::ast {::ast/type ::ast/placeholder :token curr}))

      ::token/word (parse-word parser)

      (::token/number ::token/string ::token/keyword) (parse-atom parser)

      ::token/lparen (parse-tuple-pattern parser)

      ::token/lbracket (parse-list-pattern parser)

      ::token/starthash (parse-hash-pattern parser)

      ::token/startstruct (parse-struct-pattern parser)

      ::token/error
      (panic parser (:message (current parser)) sync-pattern)

      (panic parser "Expected pattern" sync-pattern))))

(defn- parse-let-expr [parser pattern]
  (let [expr (parse-expr parser)]
    (assoc expr ::ast {::ast/type ::ast/let
                       :token (current parser)
                       :pattern (::ast pattern) :expr (::ast expr)})))

(defn- parse-assignment [parser]
  (let [assignment (expect* ::token/equals "Expected assignment" parser)
        success (:success assignment)]
    (if success
      (parse-let-expr (:parser assignment) parser)
      (panic parser "Expected assignment"))))

(defn- parse-let [parser]
  (let [pattern (parse-pattern (advance parser))]
    (parse-assignment pattern)))

(defn- parse-ref-expr [parser name]
  (let [expr (parse-expr parser)]
    (assoc expr ::ast {::ast/type ::ast/ref
                       :token (current parser)
                       :name name :expr (::ast expr)})))

(defn- parse-ref-assignment [parser name]
  (let [assignment (expect* ::token/equals "Expected assignment" (advance parser))
        success (:success assignment)]
    (if success
      (parse-ref-expr (:parser assignment) name)
      (panic parser "Expected assignment")))
  )

(defn- parse-ref [parser]
  (let [name (advance parser)]
    (if (= ::token/word (token-type name))
      (parse-ref-assignment name (::token/lexeme (current name)))
      (panic parser "Expected reference name"))))

(defn- parse-else [parser]
  (let [ast (::ast parser)
        else-kw (expect* ::token/else "Expected else clause after then" parser)
        success (:success else-kw)
        else-kw-parser (:parser else-kw)]
    (if success
      (let [expr (parse-expr else-kw-parser)
            else-expr (::ast expr)]
        (assoc expr ::ast (assoc ast :else else-expr)))
      else-kw-parser)))

(defn- parse-then [parser]
  (let [ast (::ast parser)
        then-kw (expect* ::token/then "Expected then clause after if" parser)
        success (:success then-kw)
        then-kw-parser (:parser then-kw)]
    (if success
      (let [expr (parse-expr then-kw-parser (conj sync-on ::token/else))
            then-expr (::ast expr)]
        (parse-else (accept ::token/newline (assoc expr ::ast (assoc ast :then then-expr)))))
      then-kw-parser)))

(defn- parse-if [parser]
  (let [if-expr (parse-expr (advance parser) #{::token/newline ::token/then})
        ast (assoc if-expr ::ast {::ast/type ::ast/if :token (current parser) :if (::ast if-expr)})]
    (parse-then (accept ::token/newline ast))))

(defn- parse-match-clause [parser]
  (let [pattern (if (= ::token/else (token-type parser))
                  (-> parser (advance) (assoc ::ast {::ast/type ::ast/placeholder :token (current parser)}))
                  (parse-pattern parser))
        rarrow (expect* #{::token/rarrow} "Expected arrow after pattern" pattern)]
    (if (:success rarrow)
      (let [body (parse-expr (:parser rarrow))]
        (assoc body ::ast {::ast/type ::ast/clause
                           :token (current parser)
                           :pattern (::ast pattern) :body (::ast body)}))
      (panic pattern "Expected -> in match clause. Clauses must be in the form pattern -> expression" #{::token/newline ::token/rbrace}))))

(defn- parse-match-clauses [parser]
  (loop [parser (accept-many #{::token/newline} (advance parser))
         clauses []]
    (let [curr (current parser)]
      (case (::token/type curr)
        ::token/rbrace
        (if (< 0 (count clauses))        
          (assoc (advance parser) ::ast {::ast/type ::ast/clauses :token (current parser) :clauses clauses})
          (panic parser "Expected one or more clauses" #{::rbrace}))

        ::token/newline
        (recur (accept-many #{::token/newline} parser) clauses)

        (let [clause (parse-match-clause parser)]
          (recur (accept-many #{::token/newline} clause) (conj clauses (::ast clause))))))))

(defn- parse-match [parser]
  (let [match-expr (parse-expr (advance parser) #{::token/with})
        match-header (expect* #{::token/with} "Expected with" match-expr)]
    (if (:success match-header)
      (let [clauses (:parser match-header)]
        (if (= (token-type clauses) ::token/lbrace)
          ;; match expression with one or many clauses in braces
          (let [clauses (parse-match-clauses clauses)]
            (assoc clauses ::ast {::ast/type ::ast/match
                                  :token (current parser)
                                  :expr (::ast match-expr)
                                  :clauses (get-in clauses [::ast :clauses])}))
          ;; match expression with single match clause
          (let [clause (parse-match-clause clauses)]
            (assoc clause ::ast {::ast/type ::ast/match
                                 :token (current parser)
                                 :expr (::ast match-expr)
                                 :clauses [(::ast clause)]}))))

      (panic parser "Expected with after match expression"))))

(defn- parse-loop-clause [parser]
  (if (not (= ::token/lparen (token-type parser)))
    (panic parser "Loop clauses must begin with tuple patterns")
    (let [pattern (parse-tuple-pattern parser)
          arrow (expect* #{::token/rarrow} "Expected arrow" pattern)
          body (parse-expr (:parser arrow))]
      (if (:success arrow)
        (assoc body ::ast {::ast/type ::ast/clause
                           :token (current parser)
                           :pattern (::ast pattern) :body (::ast body)})
        (panic pattern "Expected -> in loop clause. Clauses must be in the form of (pattern) -> expression")))))

(defn- parse-loop-clauses [parser]
  (loop [parser (accept-many #{::token/newline} (advance parser))
         clauses []]
    (let [curr (current parser)]
      (case (::token/type curr)
        ::token/rbrace
        (if (< 0 (count clauses))        
          (assoc (advance parser) ::ast {::ast/type ::ast/clauses :token (current parser) :clauses clauses})
          (panic parser "Expected one or more loop clauses" #{::token/rbrace}))

        ::token/newline
        (recur (accept-many #{::token/newline} parser) clauses)

        (let [clause (parse-loop-clause parser)]
          (recur (accept-many #{::token/newline} clause) (conj clauses (::ast clause))))))))

(defn- parse-loop [parser]
  (let [next (advance parser)]
    (if (= ::token/lparen (token-type next))
      (let [loop-tup (parse-tuple next)
            loop-header (expect* #{::token/with} "Expected with" loop-tup)]
        (if (:success loop-header)
          (let [clauses (:parser loop-header)]
            (if (= (token-type clauses) ::token/lbrace)
              ;; loop expression with one or many clauses in braces
              (let [clauses (parse-loop-clauses clauses)]
                (assoc clauses ::ast {::ast/type ::ast/loop
                                      :token (current parser)
                                      :expr (::ast loop-tup)
                                      :clauses (get-in clauses [::ast :clauses])}))
              ;; loop expression with single match clause
              (let [clause (parse-loop-clause clauses)]
                (assoc clause ::ast {::ast/type ::ast/loop
                                     :token (current parser)
                                     :expr (::ast loop-tup)
                                     :clauses [(::ast clause)]}))))
  
          (panic parser "Expected with after loop expression")))
      (panic parser "Expected tuple as loop expression")
      )))

(defn- parse-recur [parser]
  (let [next (advance parser)]
    (if (= ::token/lparen (token-type next))
      (let [tuple (parse-tuple next)]
        (assoc tuple ::ast {::ast/type ::ast/recur
                            :token (current parser)
                            :tuple (::ast tuple)})
        )
      (panic parser "Expected tuple after recur")
      )
    )
  )

(defn- parse-cond-clause [parser]
  (let [expr (if 
               (contains? #{::token/else ::token/placeholder} (token-type parser))
               (-> parser
                 (advance)
                 (assoc ::ast {::ast/type ::ast/atom
                               :token (current parser)
                               :value true}))
               (parse-expr parser))
        rarrow (expect* #{::token/rarrow} "Expected arrow after expression in cond clause" expr)]
    (if (:success rarrow)
      (let [body (parse-expr (:parser rarrow))]
        (assoc body ::ast {::ast/type ::ast/clause
                           :token (current parser)
                           :test (::ast expr) :body (::ast body)}))
      (panic expr "Expected -> in cond clause. Clauses must be in the form test_expression -> result_expression" #{::token/newline ::token/rbrace}))))

(defn- parse-cond-clauses [parser]
  (loop [parser (accept-many #{::token/newline} parser)
         clauses []]
    (let [curr (current parser)]
      (case (::token/type curr)
        ::token/rbrace
        (if (< 0 (count clauses))        
          (assoc (advance parser) ::ast {::ast/type ::ast/clauses :token (current parser) :clauses clauses})
          (panic parser "Expected one or more clauses" #{::rbrace}))


        ::token/newline
        (recur (accept-many #{::token/newline} parser) clauses)

        (let [clause (parse-cond-clause parser)]
          (recur (accept-many #{::token/newline} clause) (conj clauses (::ast clause))))))))

(defn- parse-cond [parser]
  (let [header 
        (expect* #{::token/lbrace} "Expected { after cond" (advance parser))]
    (if (:success header)
      (let [clauses (parse-cond-clauses (:parser header))]
        (assoc clauses ::ast {::ast/type ::ast/cond
                              :token (current parser)
                              :clauses (get-in clauses [::ast :clauses])})
        )
      (panic parser "Expected { after cond")
      )
    )
  )

(defn- parse-fn-clause [parser]
  (if (not (= ::token/lparen (token-type parser)))
    (panic parser "Function clauses must begin with tuple patterns")
    (let [pattern (parse-tuple-pattern parser)
          arrow (expect* #{::token/rarrow} "Expected arrow" pattern)
          body (parse-expr (:parser arrow))]
      (if (:success arrow)
        (assoc body ::ast {::ast/type ::ast/clause
                           :token (current parser)
                           :pattern (::ast pattern) :body (::ast body)})
        (panic pattern "Expected -> in function clause. Clauses must be in the form of (pattern) -> expression")))))

(defn- parse-fn-clauses [parser]
  (loop [parser (accept-many #{::token/newline} (advance parser))
         clauses []]
    (let [curr (current parser)]
      (case (::token/type curr)
        ::token/rbrace
        (if (< 0 (count clauses))        
          (assoc (advance parser) ::ast {::ast/type ::ast/clauses :token (current parser) :clauses clauses})
          (panic parser "Expected one or more function clauses" #{::token/rbrace}))

        ::token/newline
        (recur (accept-many #{::token/newline} parser) clauses)

        (let [clause (parse-fn-clause parser)]
          (recur (accept-many #{::token/newline} clause) (conj clauses (::ast clause))))))))

(defn- parse-named-fn [parser]
  (let [name (parse-word parser)]
    (case (token-type name)
      ::token/lparen
      (let [clause (parse-fn-clause name)]
        (assoc clause ::ast {::ast/type ::ast/fn
                             :token (current parser)
                             :name (get-in name [::ast :word])
                             :clauses [(::ast clause)]}))

      ::token/lbrace
      (let [clauses (parse-fn-clauses name)]
        (assoc clauses ::ast {::ast/type ::ast/fn
                              :token (current parser)
                              :name (get-in name [::ast :word])
                              :clauses (get-in clauses [::ast :clauses])}))

      (panic name "Expected one or more function clauses"))))

(defn- parse-fn [parser]
  (let [first (advance parser)]
    (case (::token/type (current first))
      ::token/lparen
      (let [clause (parse-fn-clause first)]
        (assoc clause ::ast {::ast/type ::ast/fn
                             :name ::ast/anon
                             :token (current parser)
                             :clauses [(::ast clause)]}))

      ::token/word (parse-named-fn first)

      (panic parser "Expected name or clause after fn"))))

(defn- parse-do [parser]
  (let [first (advance parser)]
    (loop [parser first
           exprs []]
      (let [expr (parse-expr parser)
            expr+newline (accept ::token/newline expr)
            next (token-type expr+newline)]
        (if (= ::token/pipeline next)
          (recur (advance expr+newline) (conj exprs (::ast expr)))
          (assoc expr ::ast {::ast/type ::ast/pipeline
                             :token (current parser)
                             :exprs (conj exprs (::ast expr))})
          )))))

(defn- parse-import [parser]
  (let [path (parse-atom (advance parser))
        as (expect* #{::token/as} "Expected as after path" path)
        named? (if (:success as)
                 (expect* #{::token/word} "Expected name binding after as" (:parser as))
                 nil)
        name (if (:success named?)
               (parse-word (:parser as))
               nil
               )]
    (cond
      (not= ::token/string (token-type (advance parser)))
      (panic parser "Expected path after import" #{::token/newline})

      (not (:success as))
      (panic parser "Expected as after path" #{::token/newline})

      (not (:success named?))
      (panic parser "Expected name binding after as")

      :else
      (assoc name ::ast {::ast/type ::ast/import
                         :token (current parser)
                         :path (get-in path [::ast :value])
                         :name (get-in name [::ast :word])}))))

(defn- parse-panic [parser]
  (let [expr (parse-expr (advance parser))]
    (assoc expr ::ast {::ast/type ::ast/panic
                       :token (current parser) :expr (::ast expr)})))

(defn- parse-expr
  ([parser] (parse-expr parser sync-on))
  ([parser sync-on]
   (let [token (current parser)]
     (case (::token/type token)

       (::token/number ::token/string)
       (parse-atom parser)

       ::token/keyword
       (let [next (ppeek parser)
             type (::token/type next)]
         (if (= type ::token/lparen)
           (parse-synthetic parser)
           (parse-atom parser)))

       ::token/word
       (let [next (ppeek parser)
             type (::token/type next)]
         (case type
           (::token/lparen ::token/keyword) (parse-synthetic parser)
           (parse-word parser)))

       (::token/nil ::token/true ::token/false)
       (parse-atomic-word parser)

       ::token/lparen (parse-tuple parser)

       ::token/lbracket (parse-list parser)

       ::token/startset (parse-set parser)

       ::token/starthash (parse-hash parser)

       ::token/startstruct (parse-struct parser)

       ::token/lbrace (parse-block parser)

       ::token/let (parse-let parser)

       ::token/if (parse-if parser)

       ::token/match (parse-match parser)

       ::token/fn (parse-fn parser)

       ::token/do (parse-do parser)

       ::token/cond (parse-cond parser)

       ::token/ns (parse-ns parser)

       ::token/import (parse-import parser)

       ::token/ref (parse-ref parser)

       ::token/loop (parse-loop parser)

       ::token/recur (parse-recur parser)

       ::token/panic (parse-panic parser)

       ;; TODO: improve handling of comments?
       ;; Scanner now just skips comments
       ;; ::token/comment (advance parser)

       ::token/error (panic parser (:message token) sync-on)

       (::token/rparen ::token/rbrace ::token/rbracket)
       (panic parser (str "Unbalanced enclosure: " (::token/lexeme token)))

       (::token/semicolon ::token/comma)
       (panic parser (str "Unexpected delimiter: " (::token/lexeme token)))

       (panic parser "Expected expression" sync-on)))))

(defn parse [lexed]
  (-> lexed
    (:tokens)
    (parser)
    (parse-script)))

(comment
  (def pp pp/pprint)
  (def source "

    let #{foo, :bar 23} = #{:foo 42, :bar 23}

  ")
  (def lexed (scanner/scan source))
  (def tokens (:tokens lexed))
  (def p (parser tokens))

  (println "")
  (println "")
  (println "******************************************************")
  (println "")
  (println "*** *** NEW PARSE *** ***")

  (-> p
    (parse-script)
    (::ast)
    (pp)))

(comment "
	Further thoughts/still to do:
  * Functions docstrings
  * Cond expressions
  * Loops
  * Structs
  * Namespaces
  * Types (:|)
  * Modules
  * Add `as` clauses to patterns
  * Add `when` clauses to patterns
  * var/mut
  * ref/swap
  * Splats in lists, hashmaps, sets
  * AST nodes should include tokens/locations
  - at current, only atoms do this
  * Improve error handling in hashmap parsing
  * Consider error handling in match expressions
  * Add treatment of ignored variables
  * Placeholders
  * How much in parser, how much in analysis?

  Some architectural changes:
  * UGH, this code is just kind of a mess and hard to reason about
  * Especially sequential forms
  * Parsers are hard
  * One idea:
  * Refactor everything so that it returns a success or failure
  * Because this is all stateless, in sequential forms, you can just do all the things
  * This lets you do one let (with everything building up) and then a cond with bespoke errors/panics
  * This also still lets you encapsulate parsererrors with poisoned nodes

  ")






