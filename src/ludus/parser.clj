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

(defn- peek [parser]
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
(def sync-on #{
               ::token/newline
               ::token/semicolon
               ::token/comma
               ::token/rparen
               ::token/rbracket
               ::token/rbrace
               ::token/eof
               })

(defn- sync [parser message origin end]
  (let [poison {
                ::ast/type ::ast/poison
                :message message
                :origin origin
                :end end
                }]
    (-> parser
      (assoc ::ast poison)
      (update ::errors conj poison))))

(defn- poisoned? [parser]
  (= ::ast/poison (get-in parser [::ast ::ast/type])))

(defn- panic 
  ([parser message] (panic parser message sync-on))
  ([parser message sync-on]
   (println (str "PANIC!!! in the parser: " message))
   (let [
         sync-on (conj (if (set? sync-on) sync-on #{sync-on}) ::token/eof)
         origin (current parser)
         ]
     (loop [parser parser]
       (let [
             curr (current parser)
             type (::token/type curr)
             ]
         (if (or (at-end? parser) (contains? sync-on type))
           (sync parser message origin curr) 
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
      {:success false :parser (panic (advance parser) message)}
      )))

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
      (let [
            curr (current parser)
            type (::token/type curr)]
        (if (contains? tokens type)
          (recur (advance parser))
          parser)))))

;; various parsing functions
(defn- parse-atom [parser]
  (let [token (current parser)]
    (-> parser
      (advance)
      (assoc ::ast {
                    ::ast/type ::ast/atom 
                    :token token
                    :value (::token/literal token)}))))

;; just a quick and dirty map to associate atomic words with values
(def atomic-words {
                   ::token/nil nil
                   ::token/true true
                   ::token/false false})

(defn parse-atomic-word [parser]
  (let [token (current parser)]
    (-> parser
      (advance)
      (assoc ::ast {
                    ::ast/type ::ast/atom
                    :token token
                    :value (get atomic-words (::token/type token))}))))


(defn- add-member [members member]
  (if (nil? member)
    members
    (conj members member)))

(defn- parse-tuple [origin]
  (loop [
         parser (accept-many #{::token/newline ::token/comma} (advance origin))
         members []
         current_member nil
         ]
    (let [curr (current parser)]
      (case (token-type parser)
        ::token/rparen (let [ms (add-member members current_member)]
                         (assoc (advance parser) ::ast 
                           {::ast/type ::ast/tuple
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

        (let [parsed (parse-expr parser #{::token/comma ::token/newline ::token/rparen})]
          (recur parsed members (::ast parsed)))))))

(defn- parse-list [origin]
  (loop [
         parser (accept-many #{::token/newline ::token/comma} (advance origin))
         members []
         current_member nil
         ]
    (let [curr (current parser)]
      (case (token-type parser)
        ::token/rbracket (let [ms (add-member members current_member)]
                           (assoc (advance parser) ::ast 
                             {::ast/type ::ast/list
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
  (loop [
         parser (accept-many #{::token/newline ::token/comma} (advance origin))
         members []
         current_member nil
         ]
    (let [curr (current parser)]
      (case (token-type parser)
        ::token/rbrace (let [ms (add-member members current_member)]
                         (assoc (advance parser) ::ast 
                           {::ast/type ::ast/set
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
  (loop [
         parser (accept-many #{::token/newline ::token/comma} (advance origin))
         members {}
         current_member nil
         ]
    (let [curr (current parser)]
      (case (token-type parser)
        ::token/rbrace (let [ms (add-member members current_member)]
                         (assoc (advance parser) ::ast 
                           {::ast/type ::ast/hash
                            :members ms}))

        (::token/comma ::token/newline)
        (recur 
          (accept-many #{::token/comma ::token/newline} parser) 
          (add-member members current_member) nil)

        (::token/rbracket ::token/rparen)
        (panic parser (str "Mismatched enclosure in set: " (::token/lexeme curr)))

        ::token/eof
        (panic (assoc origin ::errors (::errors parser)) "Unterminated set" ::token/eof)

        ::token/word
        (if (not current_member) (let [parsed (parse-word parser) word (get-in parsed [::ast :word])]
                  (recur parsed members {(keyword word) (::ast parsed)}))
          (panic parser "Hashmap entries must be single words or keyword+expression pairs." #{::token/rbrace})
        )

        ::token/keyword
        (if (not current_member) (let [kw (parse-atom parser) expr (parse-expr kw #{::token/comma ::token/newline ::token/rbrace})]
                  (println "found keyword/expr pair:" (:value kw))
                  (pp/pprint (::ast expr))
                  (recur expr members {(:value (::ast kw)) (::ast expr)}))
          (panic parser "Hashmap entries must be single words or keyword+expression pairs." #{::token/rbrace})
        )

        (panic parser "Hashmap entries must be single words or keyword+expression pairs" #{::token/rbrace})))))

(defn- parse-block [origin]
  (loop [
         parser (accept-many #{::token/newline ::token/semicolon} (advance origin))
         exprs []
         current_expr nil
         ]
    (let [curr (current parser)]
      (case (token-type parser)
        ::token/rbrace (let [es (add-member exprs current_expr)]
                         (if (empty? es)
                           (advance (panic parser "Blocks must have at least one expression"))
                           (assoc (advance parser) ::ast {
                                                          ::ast/type ::ast/block
                                                          :exprs es
                                                          })))

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

(defn parse-script [parser]
  (loop [
         parser (accept-many #{::token/newline ::token/semicolon} parser)
         exprs []
         current_expr nil
         ]
      (case (token-type parser)
        ::token/eof (let [es (add-member exprs current_expr)]
                      (if (empty? es)
                        (panic parser "Scripts must have at least one expression")
                        (assoc parser ::ast {::ast/type ::ast/script :exprs es})))

        (::token/semicolon ::token/newline)
        (recur 
          (accept-many #{::token/semicolon ::token/newline} parser) 
          (add-member exprs current_expr)
          nil)

        (let [parsed
              (if current_expr
                (panic parser "Expected end of expression" #{::token/semicolon ::token/newline})
                (parse-expr parser)
                )
              ]
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
        (let [parsed (parse-tuple parser)]
          (recur parsed (conj terms (::ast parsed))))

        (-> parser 
          (assoc ::ast {::ast/type ::ast/synthetic :terms terms})

          )))))

(defn- parse-word [parser]
  (let [curr (current parser)]
    (-> parser
      (advance)
      (assoc ::ast {::ast/type ::ast/word :word (::token/lexeme curr)}))))

(def sync-pattern (s/union sync-on #{::token/equals ::token/rarrow}))

(defn- parse-tuple-pattern [origin]
  (loop [
         parser (accept-many #{::token/newline ::token/comma} (advance origin))
         members []
         current_member nil
         ]
    (let [curr (current parser)]
      (case (token-type parser)
        ::token/rparen (let [ms (add-member members current_member)]
                         (assoc (advance parser) ::ast 
                           {::ast/type ::ast/tuple
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
      ::token/placeholder (-> parser 
        (advance)
        (assoc ::ast {::ast/type ::ast/placeholder}))

      ::token/word (parse-word parser)

      (::token/number ::token/string ::token/keyword) (parse-atom parser)

      ::token/lparen (parse-tuple-pattern parser)

      ::token/error
      (panic parser (:message (current parser)) sync-pattern)

      (panic parser "Expected pattern" sync-pattern)
      )))

(defn- parse-let-expr [parser pattern]
  (let [expr (parse-expr parser)]
    (assoc expr ::ast {::ast/type ::ast/let
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
        ast (assoc if-expr ::ast {::ast/type ::ast/if :if (::ast if-expr)})]
    (parse-then (accept ::token/newline ast))
    ))

(defn- parse-expr 
  ([parser] (parse-expr parser sync-on))
  ([parser sync-on] (let [token (current parser)]
                      (case (::token/type token)
  
                        (::token/number ::token/string)
                        (parse-atom parser)
  
                        ::token/keyword (let [next (peek parser)
                                              type (::token/type next)]
                                          (if (= type ::token/lparen)
                                            (parse-synthetic parser)
                                            (parse-atom parser)))
  
                        ::token/word (let [next (peek parser)
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
  
                        ::token/lbrace (parse-block parser)
  
                        ::token/let (parse-let parser)
  
                        ::token/if (parse-if parser)
  
                        ::token/error (panic parser (:message token) sync-on)
  
                        (::token/rparen ::token/rbrace ::token/rbracket)
                        (panic parser (str "Unbalanced enclosure: " (::token/lexeme token)))
  
                        (::token/semicolon ::token/comma)
                        (panic parser (str "Unexpected delimiter: " (::token/lexeme token)))
  
                        (panic parser "Expected expression" sync-on)
  
                        ))))

(defn parse [lexed]
  (-> lexed
    (:tokens)
    (parser)
    (parse-script)
))

(do
  (def pp pp/pprint)
  (def source "#{:a :b, :c :d}")
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
    (pp)
    )
  )

(comment "
	Further thoughts/still to do:
  * AST nodes should include tokens/locations
    - at current, only atoms do this
  * Improve error handling in hashmap parsing
  * 

	Other quick thoughts:

	* Placeholders

		* Does this want to happen in parsing or in analysis?

	For future correctness checks:

")






