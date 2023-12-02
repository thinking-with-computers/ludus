(ns ludus.prelude
 	#?(:cljs (:require [shadow.resource :as r]))
 	)

(def prelude
 	#?(
   		:clj (slurp "src/ludus/prelude.ld")
   		:cljs (r/inline "prelude.ld")
    	))