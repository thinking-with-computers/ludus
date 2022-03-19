(ns ludus.interpreter
	(:require
		[ludus.parser :as parser]
		[ludus.scanner :as scanner]
		[ludus.ast :as ast]
		[ludus.collections :as colls]
		[clojure.pprint :as pp]))

;; right now this is not very efficient:
;; it's got runtime checking
;; we should be able to do these checks statically
;; that's for later, tho
(defn- resolve [word ctx-atom]
	(let [ctx @ctx-atom]
		(if (contains? ctx word)
			(get ctx word)
			(if (contains? ctx ::parent)
				(recur word (::parent ctx))
				(throw (new Exception (str "Unbound name: " word)))))))

(declare interpret match)

(defn- match-tuple [pattern value ctx-atom]
	(cond
		(not (vector? value)) {:success false :reason "Could not match non-tuple value to tuple"}

		(not (= ::colls/tuple (first value))) {:success false :reason "Could not match list to tuple"}

		(not (= (:length pattern) (dec (count value))))
		{:success false :reason "Cannot match tuples of different lengths"}

		(= 0 (:length pattern) (dec (count value))) {:success true :ctx {}}

		:else (let [members (:members pattern)]
			(loop [i (dec (:length pattern))
				ctx {}]
				(if (= 0 i)
					{:success true :ctx ctx}
					(let [match? (match (nth members i) (nth value (inc i)) ctx-atom)]
						(if (:success match?)
							(recur (dec i) (merge ctx (:ctx match?)))
							{:success false :reason (str "Could not match " pattern " with " value)}
						))
				)

				))
		))

(defn- match [pattern value ctx-atom]
	(let [ctx @ctx-atom]
		(case (::ast/type pattern)
			::ast/placeholder {:success true :ctx {}}

			::ast/atom
			(let [match-value (:value pattern)]
				(if (= match-value value)
					{:success true :ctx {}}
					{:success false 
						:reason (str "No match: Could not match " match-value " with " value)}))
	
			::ast/word
			(let [word (:word pattern)]
				(if (contains? ctx word)
					{:success false :reason (str "Name " word " is already bound")}
					{:success true :ctx {word value}}
					))

			::ast/tuple (match-tuple pattern value ctx-atom)
	
			(do
				(println "ERROR! Unexpected pattern:")
				(pp/pprint pattern)
			)
			)))

(defn- update-ctx [ctx new-ctx]
	(merge ctx new-ctx))

;; TODO: get "if let" pattern working
;; TODO: get typed exceptions to distinguish panics
(defn- interpret-let [ast ctx]
	(let [pattern (:pattern ast)
		expr (:expr ast)
		value (interpret expr ctx)
		match (match pattern value ctx)
		success (:success match)]
		(if success
			(swap! ctx update-ctx (:ctx match))
			(throw (new Exception (:reason match))))
		value
		))

(defn- interpret-if [ast ctx]
	(let [if-expr (:if ast)
		then-expr (:then ast)
		else-expr (:else ast)
		if-value (interpret if-expr ast)]
		(if if-value
			(interpret then-expr ctx)
			(interpret else-expr ctx)
			)))

(defn interpret [ast ctx]
	(case (::ast/type ast)

		::ast/atom (:value ast)

		::ast/word (resolve (:word ast) ctx)

		::ast/let (interpret-let ast ctx)

		::ast/if (interpret-if ast ctx)

		::ast/block
		(let [exprs (:exprs ast)
			inner (pop exprs)
			last (peek exprs)
			ctx (atom {::parent ctx})]
			(run! #(interpret % ctx) inner)
			(interpret last ctx)
			)

		::ast/script
		(let [exprs (:exprs ast)
			inner (pop exprs)
			last (peek exprs)
			ctx (atom ctx)
			]
			(run! #(interpret % ctx) inner)
			(interpret last ctx)
			)

		;; note that the runtime representations of collections is
		;; unboxed in the tree-walk interpreter
		;; tuples & lists are both vectors, the first element
		;; distinguishes them
		::ast/tuple
		(let [members (:members ast)]
			(into [::colls/tuple] (map #(interpret % ctx)) members))

		::ast/list
		(let [members (:members ast)]
			(into [::colls/list] (map #(interpret % ctx)) members))

		::ast/set
		(let [members (:members ast)]
			(into #{} (map #(interpret % ctx)) members))

		(do 
			(println "ERROR! Unexpected AST node:")
			(pp/pprint ast)
			)

		))


(do

(def source "
	let (:foo, 1, :bar) = (:foo, 1, :bar) 
")

(println "")
(println "****************************************")
(println "*** *** NEW INTERPRETATION *** ***")
(println "")

(-> source
	(scanner/scan)
	(parser/parse) 
	(::parser/ast)
	(interpret {})
	(pp/pprint)
))