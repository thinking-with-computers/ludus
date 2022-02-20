(ns ludus.parser
  (:require 
    [ludus.token :as token]
    [ludus.scanner :as scanner]
    [ludus.ast :as ast]
    [clojure.pprint :as pp]))

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

;; some forward declarations
(declare parse-expr)
(declare parse-word)

;; handle some errors
(defn- sync [parser message origin end]
  (println "Synching on " (current parser))
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

(defn- panic [parser message sync-on]
  (println "PANIC!!! in the parser")
  (let [origin (current parser)]
    (loop [parser parser]
      (let [
            curr (current parser)
            type (::token/type curr)
            ]
        (if (or (= ::token/eof type) (contains? sync-on type))
          (sync parser message origin curr) 
          (recur (advance parser)))))))

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

(defn- parse-tuple [parser]
  (loop [parser (advance parser)
         members []
         current_member nil]
    (let [curr (current parser)]
      (case (::token/type curr)
        ::token/rparen (let [ms (add-member members current_member)] 
                         (assoc (advance parser) ::ast 
                           {::ast/type ::ast/tuple
                            :length (count ms)
                            :members ms}))
        (::token/comma ::token/newline) (recur (advance parser) (add-member members current_member) nil)

        (let [parsed (parse-expr parser)]
          (if (= ::ast/poison (get-in parsed [::ast ::ast/type]))
            (panic parsed (:message (::ast parsed)) #{::token/rparen})
            (recur parsed members (::ast parsed))
            )
          )

        ))))

(defn- parse-list [parser]
  (loop [parser (advance parser)
         members []
         current_member nil]
    (let [curr (current parser)]
      (case (::token/type curr)
        ::token/rbracket (let [ms (add-member members current_member)] 
                           (assoc (advance parser) ::ast 
                             {::ast/type ::ast/list
                              :members ms}))
        (::token/comma ::token/newline) (recur (advance parser) (add-member members current_member) nil)

        (let [parsed (parse-expr parser)]
          (recur parsed members (::ast parsed)))

        ))))

(defn- parse-set [parser]
  (loop [parser (advance parser)
         members []
         current_member nil]
    (let [curr (current parser)]
      (case (::token/type curr)
        ::token/rbrace (let [ms (add-member members current_member)] 
                         (assoc (advance parser) ::ast 
                           {::ast/type ::ast/set
                            :members ms}))
        (::token/comma ::token/newline) (recur (advance parser) (add-member members current_member) nil)

        (let [parsed (parse-expr parser)]
          (recur parsed members (::ast parsed)))

        ))))

(defn- parse-block [parser]
  (loop [parser (advance parser)
         exprs []
         current_expr nil]
    (case (::token/type (current parser))
      ::token/rbrace 
      (assoc (advance parser) ::ast
        (if (and (empty? exprs) (nil? current_expr))
          {::ast/type ::ast/poison :message "Blocks must have at least one expression"}
          {::ast/type ::ast/block :exprs (add-member exprs current_expr)}))
			
      (::token/semicolon ::token/newline) 
      (recur (advance parser) (add-member exprs current_expr) nil)
			
      (if current_expr
        (-> parser
          (advance)
          (assoc ::ast {::ast/type ::ast/poison :message "Expected end of expression"}))
        (let [parsed (parse-expr parser)]
          (recur parsed exprs (::ast parsed))))
      )))

(defn- parse-script [parser]
  (loop [parser parser
         exprs []
         current_expr nil]
    (comment (println "*** Parsing script")
      (print "Exprs: ")
      (pp/pprint exprs)
      (print "Current expr: ")
      (pp/pprint current_expr)
      (println "Current token type " (::token/type (current parser))))
    (case (::token/type (current parser))
      ::token/eof (assoc parser ::ast
                    {::ast/type ::ast/script :exprs (add-member exprs current_expr)})

      (::token/semicolon ::token/newline)
      (recur (advance parser) (add-member exprs current_expr) nil)

      (if current_expr
        (if (poisoned? current_expr)
          (panic parser (:message current_expr) #{::token/newline ::token/semicolon})
          (let [synced (panic parser "Expected end of expression" #{::token/newline ::token/semicolon})]
            (recur synced exprs (::ast synced))
            ))
        (let [parsed (parse-expr parser)]
          (recur parsed exprs (::ast parsed))
          ))

      )))

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

(defn- parse-pattern [parser]
  (let [curr (current parser)
        type (::token/type curr)]
    (case type
      ::token/word (parse-word parser)

      (::token/number ::token/string ::token/keyword) (parse-atom parser)

      (-> parser
        (advance)
        (assoc ::ast {::ast/type ::ast/poison :message "Expected pattern"}))
      )))

(defn- expect [token message parser]
  (let [curr (current parser)
        type (::token/type curr)]
    (if (= type token)
      (advance parser)
      (-> parser
        (advance)
        (assoc ::ast {::ast/type ::ast/poison :message message})))))

(defn- accept [token parser]
  (let [curr (current parser)
        type (::token/type curr)]
    (if (= type token)
      (advance parser)
      parser)))

(defn- accept-many [token parser]
  (loop [curr (current parser)]
    (let [type (::token/type curr)]
      (if (= type token)
        (recur (advance parser))
        parser))))


(defn- parse-let [parser]
  (let [
        pattern (parse-pattern (advance parser))
        equals (expect ::token/equals "Expected assignment" pattern)
        expr (parse-expr equals)
        results (map #(get-in % [::ast ::ast/type]) [pattern equals expr])
        ]
    (if (some #(= ::ast/poison %) results)
      (println ::poison)
      (assoc expr ::ast {
                         ::ast/type ::ast/let 
                         :pattern (::ast pattern)
                         :expr (::ast expr)}))
    ))

(defn- parse-if [parser]
  (let [
        if-expr (parse-expr (advance parser))
        then (expect ::token/then "Expected then" (accept ::token/newline if-expr))
        then-expr (parse-expr then)
        else (expect ::token/else "Epected else" (accept ::token/newline then-expr))
        else-expr (parse-expr else)
        results (map #(get-in % [::ast ::ast/type]) [if-expr then then-expr else else-expr])
        ]
    (if (some #(= ::ast/poison %) results)
      (println ::ast/poison)
      (assoc else-expr ::ast {
                              ::ast/type ::ast/let
                              :if-expr (::ast if-expr)
                              :then-expr (::ast then-expr)
                              :else-expr (::ast else-expr)
                              }))
    ))

(def expr-sync #{
                 ::token/newline
                 ::token/semicolon
                 ::token/comma
                 ::token/rparen
                 ::token/rbracket
                 ::token/rbrace
                 })

(defn- parse-expr [parser]
  (let [token (current parser)]
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

      ::token/let (parse-let parser)

      ::token/if (parse-if parser)

      ::token/error (panic parser (:message token) 
                      #{
                        ::token/newline
                        ::token/semicolon
                        ::token/comma
                        ::token/rparen
                        ::token/rbracket
                        ::token/rbrace
                        })

      (::token/rparen ::token/rbrace ::token/rbracket)
      (panic parser (str "Unbalanced enclosure: " (::token/lexeme token)) expr-sync)

      (::token/semicolon ::token/comma)
      (panic parser (str "Unexpected delimiter: " (::token/lexeme token)) expr-sync)

      (panic parser "Expected expression" expr-sync)

      )))

(do
  (def pp pp/pprint)
  (def source "(foo, bar, baz^, } )
    :foo (bar)

    [1, 2, 3]")
  (def lexed (scanner/scan source))
  (def tokens (:tokens lexed))
  (def p (parser tokens))

  (-> p
    (parse-script)
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
    - this works (ish) for expr, script, tuple
    - add to everything else
    - investigate duplicated/missing error messages

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






