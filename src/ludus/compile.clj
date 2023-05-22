(ns ludus.compile
 	(:require
  		[ludus.grammar :as g]
  		[ludus.parser-new :as p]
  		[ludus.scanner :as s]))

(def source 
  "1"
  )

(def result (->> source s/scan :tokens (p/apply-parser g/script)))

(println result)

(comment "
	What sorts of compiling and validation do we want to do? Be specific.

	- check used names are bound (validation)
	- check bound names are free (validation)
	- check `recur` is only ever in `loop` (and in `fn` bodies?), in tail position (validation)
	- separate function arities into different functions (optimization)
	- desugar partially applied functions (?) (simplification)
	- desugar keyword entry shorthand (?) (simplification)
	- flag tail calls for optimization (optimization)
		- direct tail calls
		- through different expressions
			- block
			- if
			- cond
			- match
			- let
	- check ns access (validation)
	- check constraints: only use specific fns (checked against a constraint-specific ctx) (validation)

	")