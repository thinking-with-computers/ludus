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

(defn- expect [message tokens parser]
	(let [curr (current parser)]
		(if (contains? tokens (::token/type curr))
			parser
			(assoc parser ::ast {
				::ast/type ::ast/poison 
				:message (str "Expected " message)
				:token curr}))))

(defn- expect+ [message tokens parser]
	(print "Epxecting ")
	(pp/pprint tokens)
	(let [curr (current parser)
		  next (next parser)]
		(println "Current: " curr " Next: " next)
		(if (contains? tokens (::token/type curr))
			(if (contains? tokens (::token/type next))
				(recur message tokens (advance parser))
				parser)
			(assoc parser ::ast {
				::ast/type ::ast/poison 
				:message (str "Expected " message)
				:token curr}))))

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

(declare parse-expr)

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
					{::ast/type ::ast/block :exprs (add-member exprs current_expr)})
			
			(::token/semicolon ::token/newline) 
				(recur (advance parser) (add-member exprs current_expr) nil)
			
			(let [parsed (parse-expr parser)]
				(recur parsed exprs (::ast parsed)))
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

			(let [parsed (parse-expr parser)]
				(recur parsed exprs (::ast parsed)))

			)))

(defn- parse-expr [parser]
	(loop [parser parser]
		(let [token (current parser)]
			(println "Parsing expr " (::token/type token))
			(case (::token/type token)

				(::token/number ::token/string ::token/keyword)
					(parse-atom parser token)

				(::token/nil ::token/true ::token/false)
					(parse-atomic-word parser token)

				::token/lparen (parse-tuple parser)

				::token/lbracket (parse-list parser)

				::token/startset (parse-set parser)

				::token/lbrace (parse-block parser)

				))))

(def source "{1; 2; :foo}\n{1}")

(def tokens (:tokens (scanner/scan source)))

(def p (parser tokens))

(pp/pprint p)

(-> (parse-script p)
	(::ast)
	(:exprs))

(comment "
	Just leaving this note here for myself. I'm too tired to write more code, but I want not to lose momentum. So:

	Next, we'll need to look at synthetic expressions. Their first terms are always keywords or words.

	Take the case of keywords:
	* if the next token is NOT a (, then return the atomic keyword
	* if it is, then you're in a synthetic expression

	Take the case of words:
	* if the next token is NOT : or (, then return the atomic word
	* if it is, then you're in a synthetic expression

	A synthetic expression is a chain of calls and property accesses.
	That means you can parse, starting from either a keyword or a word, any combination of tuples and keywords, in any order.
	Until, that is, you reach an expression terminator (of any kind): eof newline ; } ) 
	Tuples of this kind are special in each case:
	* Tuples after *initial* keywords may only be of length 1 (b/c that's a called keyword; subsequent keywords in synthetic expressions are property accesses)
	* All other tuples may contain exactly one or zero placeholders as members
	* We want both of these to be parse errors (should these be at parsing or subsequent correctness checks)

	Other quick thoughts:
	* Blocks must have at least one expression. This should be a parse error.
	* `let`s with literal matching and word matching are easy. Parsing all the patterns comes later.
	* The first conditional form to parse is `if` (b/c no patterns) (but it does have funny scoping!)
	* Once I get this far, then it's time to wire up the interpreter (with hard-coded functions, and the beginning of static analysis)

	* ALSO: time to start working on parsing errors. (poisoned nodes, panic mode, etc.)

	For future correctness checks:
	* Early (even as part of wiring up the interpreter), begin the static analysis check for unbound names, redeclaration
	* Compound `loop` and `gen` forms must have LHS's (tuple patterns) of the same length
")






