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
(declare parse-expr)
(declare parse-word)

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

        (let [parsed (parse-expr parser)]
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

        (let [parsed (parse-expr parser)]
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

        (let [parsed (parse-expr parser)]
          (recur parsed members (::ast parsed)))))))

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
                           (panic parser "Blocks must have at least one expression")
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

(defn- parse-script [parser]
  (loop [
         parser (accept-many #{::token/newline ::token/semicolon} parser)
         exprs []
         current_expr nil
         ]
    (let [curr (current parser)]
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
          (recur parsed exprs (::ast parsed)))))))

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

(defn- parse-pattern [parser]
  (let [curr (current parser)
        type (::token/type curr)]
    (case type
      ::token/word (parse-word parser)

      (::token/number ::token/string ::token/keyword) (parse-atom parser)

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
    (if (poisoned? pattern)
      (panic (advance parser) "Expected pattern")
      (parse-assignment pattern)
      )
    ))

(defn- parse-let* [parser]
  (let [pattern (parse-pattern (advance parser))]
    (parse-assignment pattern)))

(defn- parse-if* [parser]
  )

;; TODO: Fix failure case here 
(defn- parse-if [parser]
  (let [
        if-expr (parse-expr (advance parser) #{::token/then ::token/newline})
        then (expect ::token/then "Expected then" (accept ::token/newline if-expr))
        then-expr (parse-expr then)
        else (expect ::token/else "Epected else" (accept ::token/newline then-expr))
        else-expr (parse-expr else #{::token/else ::token/newline})
        results (map #(get-in % [::ast ::ast/type]) [if-expr then then-expr else else-expr])
        ]
    (if (some #(= ::ast/poison %) results)
      (println ::ast/poison) ;; TODO: FIX THIS
      (assoc else-expr ::ast {
                              ::ast/type ::ast/if
                              :if-expr (::ast if-expr)
                              :then-expr (::ast then-expr)
                              :else-expr (::ast else-expr)
                              }))
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
  
        ::token/lbrace (parse-block parser)
  
        ::token/let (parse-let* parser)
  
        ::token/if (parse-if parser)
  
        ::token/error (panic parser (:message token))
  
        (::token/rparen ::token/rbrace ::token/rbracket)
        (panic parser (str "Unbalanced enclosure: " (::token/lexeme token)))
  
        (::token/semicolon ::token/comma)
        (panic parser (str "Unexpected delimiter: " (::token/lexeme token)))
  
        (panic parser "Expected expression" sync-on)
  
        ))))

(do
  (def pp pp/pprint)
  (def source "if 
    then bar 
    else baz")
  (def lexed (scanner/scan source))
  (def tokens (:tokens lexed))
  (def p (parser tokens))

  (println "")
  (println "")
  (println "******************************************************")
  (println "")
  (println "*** *** NEW PARSE *** ***")

  (-> p
    (parse-if)
    (::ast)
    (pp)
    )
  )

(comment "
	Further thoughts/still to do:
	* Clean up the parsing functions:
		- use accept-many in blocks and scripts
		- ast nodes should include their tokens (this is added for atoms, which may be fully sufficient)
	* Time to start working on parsing errors (poisoned nodes, panic mode, etc.)
    - this now works on atoms, collections, synthetics, blocks, and scripts
    - add to everything else:
    - CURRENTLY:
      * figure out how to parse `if` with similar strategy to `let`
        * one possibility is to paramterize parse-expr with a sync token:
        the idea here is that exprs will have different end sync points in different contexts (`}` vs `then` vs `\n`, etc.)
      * the strategy here will be the same for the rest of the sequential constructs: `let`, `if`, `import`, `fn`, `loop`, etc.

	Other quick thoughts:
	* Once I get this far, then it's time to wire up the interpreter (with hard-coded functions, and the beginning of static analysis)

	* Placeholders
		* Placeholders may only appear in tuples in synthetic expressions
		* Each of these may have zero or one placeholders
		* Does this want to happen in parsing or in analysis?

	For future correctness checks:
	* Early (even as part of wiring up the interpreter), begin the static analysis check for 
		- unbound names 
		- re-binding of names
	* Compound `loop` and `gen` forms must have LHS's (tuple patterns) of the same length
	* Recur must be in tail position in `loop`s
	* Tail call optimization for simple recursion
	* Check arities for statically known functions
	* Enforce single-member tuple after called keywords
")






