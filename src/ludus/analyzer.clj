(ns ludus.analyzer
  (:require
   [ludus.ast :as ast]
   [ludus.token :as token]))

(defn analyze [ast] ast)

(comment "
	Here's where we do a bunch of static analysis.
	Some things we might wish for:
	* No unused bindings
	* No unbound names
	* Compound `loop` and `gen` forms must have LHS's (tuple patterns) of the same length
	* Recur must be in tail position in `loop`s
	* Tail call optimization for simple recursion (rewrite it as a loop?)
	* Check arities for statically known functions
	* Enforce single-member tuple after called keywords
	* Placeholders may only appear in tuples in synthetic expressions
	* Each of these may have zero or one placeholders
	* Arity of called keywords must be 1
")