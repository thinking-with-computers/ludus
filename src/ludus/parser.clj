(ns ludus.parser
	(:require 
		[ludus.token :as token]
		[ludus.scanner :as scanner]
		[ludus.ast :as ast]
		[clojure.pprint :as pp]))

(defn- parser [tokens]
	{::tokens tokens ::token 0 ::ast {}})

(defn- current [parser]
	(nth (::tokens parser) (::token parser) nil))

(defn- next [parser]
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

(defn- parse-atom [parser token]
	(-> parser
		(advance)
		(assoc ::ast {
			::ast/type ::ast/atom 
			:value (::token/literal token)})))

;; just a quick and dirty map to associate atomic words with values
(def atomic-words {
	::token/nil nil
	::token/true true
	::token/false false})

(defn parse-atomic-word [parser token]
	(-> parser
		(advance)
		(assoc ::ast {
			::ast/type ::ast/atom 
			:value (get atomic-words (::token/type token))})))


(defn- add-member [members member]
	(if (nil? member)
			members
			(conj members member)))

(defn- parse-tuple [parser]
	(loop [parser (advance parser)
		members []
		current_member nil]
		(println "***parsing tuple with ")
		(print "parser ") (pp/pprint parser)
		(print "members ") (pp/pprint members)
		(print "current member ")(pp/pprint current_member)
		(let [curr (current parser)]
			(case (::token/type curr)
				::token/rparen (let [ms (add-member members current_member)] 
					(assoc (advance parser) ::ast 
						{::ast/type ::ast/tuple
							:length (count ms)
							:members ms}))
				(::token/comma ::token/newline) (recur (advance parser) (add-member members current_member) nil)

				(let [parsed (parse-expr parser)]
					(print "Got to expr with")
					(pp/pprint (::ast parsed))
					(recur parsed members (::ast parsed)))

				))))

(defn- parse-list [parser]
	(loop [parser (advance parser)
		members []
		current_member nil]
		(println "***parsing list with ")
		(print "parser ") (pp/pprint parser)
		(print "members ") (pp/pprint members)
		(print "current member ")(pp/pprint current_member)
		(let [curr (current parser)]
			(case (::token/type curr)
				::token/rbracket (let [ms (add-member members current_member)] 
					(assoc (advance parser) ::ast 
						{::ast/type ::ast/list
							:members ms}))
				(::token/comma ::token/newline) (recur (advance parser) (add-member members current_member) nil)

				(let [parsed (parse-expr parser)]
					(print "Got to expr with ") (pp/pprint (::ast parsed))
					(recur parsed members (::ast parsed)))

				))))

(defn- parse-set [parser]
	(loop [parser (advance parser)
		members []
		current_member nil]
		(println "***parsing set with ")
		(print "parser ") (pp/pprint parser)
		(print "members ") (pp/pprint members)
		(print "current member ")(pp/pprint current_member)
		(let [curr (current parser)]
			(case (::token/type curr)
				::token/rbrace (let [ms (add-member members current_member)] 
					(assoc (advance parser) ::ast 
						{::ast/type ::ast/set
							:members ms}))
				(::token/comma ::token/newline) (recur (advance parser) (add-member members current_member) nil)

				(let [parsed (parse-expr parser)]
					(print "Got to expr with ") (pp/pprint (::ast parsed))
					(recur parsed members (::ast parsed)))

				))))

(defn- parse-block [parser]
	(println "*** *** parsing block")
	(loop [parser (advance parser)
		exprs []
		current_expr nil]
		(print "current ")(pp/pprint (current parser))
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
	(println "*** *** *** parsing script")
	(loop [parser parser
		exprs []
		current_expr nil]
		(case (::token/type (current parser))
			::token/eof (assoc parser ::ast
				{::ast/type ::ast/script :exprs (add-member exprs current_expr)})

			(::token/semicolon ::token/newline)
				(recur (advance parser) (add-member exprs current_expr) nil)

			(if current_expr
				(-> parser
					(advance)
					(assoc ::ast {::ast/type ::ast/poison :message "Expected end of expression"}))
				(let [parsed (parse-expr parser)]
					(recur parsed exprs (::ast parsed))))

			)))

(defn- parse-synthetic [parser]
	(loop [parser parser
		terms []]
		(let [curr (current parser)
			type (::token/type curr)]
			(case type
				::token/keyword 
					(recur (advance parser) (conj terms (::ast (parse-atom parser curr))))

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

			(::token/number ::token/string ::token/keyword) (parse-atom parser curr)

			(-> parser
				(advance)
				(assoc ::ast {::ast/type ::ast/poison :message "Expected pattern"}))
		)))

(defn- parse-equals [parser]
	(let [curr (current parser)
		type (::token/type curr)]
		(case type
			::token/equals (advance parser)

			(-> parser
				(advance)
				(assoc ::ast {::ast/type ::ast/poison :message "Expected assignment"}))
			)))

(defn- parse-let [parser]
	(let [
		pattern (parse-pattern (advance parser))
		equals (parse-equals pattern)
		expr (parse-expr equals)
		results (map #(get-in % [::ast ::ast/type]) [pattern equals expr])
		]
		(println "!!!!!!!! let results")
		(pp/pprint results)
		))

(defn- parse-expr [parser]
	(loop [parser parser]
		(let [token (current parser)]
			(println "Parsing expr " (::token/type token))
			(case (::token/type token)

				(::token/number ::token/string)
					(parse-atom parser token)

				::token/keyword (let [next (next parser)
					type (::token/type next)]
					(if (= type ::token/lparen)
						(parse-synthetic parser)
						(parse-atom parser token)))

				::token/word (let [next (next parser)
					type (::token/type next)]
					(case type
						(::token/lparen ::token/keyword) (parse-synthetic parser)
						(parse-word parser)))

				(::token/nil ::token/true ::token/false)
					(parse-atomic-word parser token)

				::token/lparen (parse-tuple parser)

				::token/lbracket (parse-list parser)

				::token/startset (parse-set parser)

				::token/lbrace (parse-block parser)

				::token/let (parse-let parser)

				(-> parser
					(advance)
					(assoc ::ast {::ast/type ::ast/poison :message "Expected expression"}))

				))))

(def source "let {1} = 12")

(def tokens (:tokens (scanner/scan source)))

(def p (parser tokens))

(pp/pprint p)

(-> (parse-let p)
	(::ast))

(comment "
	Further thoughts/still to do:
	* Placeholders
		* Placeholders may only appear in tuples in synthetic expressions
		* Each of these may have zero or one placeholders

	Other quick thoughts:
	* `let`s with literal matching and word matching are easy. Parsing all the patterns comes later.
	* The first conditional form to parse is `if` (b/c no patterns) (but it does have funny scoping!)
	* Once I get this far, then it's time to wire up the interpreter (with hard-coded functions, and the beginning of static analysis)

	* ALSO: time to start working on parsing errors. (poisoned nodes, panic mode, etc.)

	For future correctness checks:
	* Early (even as part of wiring up the interpreter), begin the static analysis check for unbound names, redeclaration
	* Compound `loop` and `gen` forms must have LHS's (tuple patterns) of the same length
	* Recur is in tail position in `loop`s
	* Tail call optimization for simple recursion
	* Check arities for statically known functions
	* Enforce single-member tuple after called keywords
")






