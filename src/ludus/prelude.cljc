(ns ludus.prelude
 	#?(:cljs (:require [shadow.resource :as r]))
 	)

(def prelude
 	#?(
   		:clj (slurp "src/ludus/prelude.ld")
   		:cljs (r/inline "./prelude.ld")
    	))

(def postlude
 	#?(
   		:clj (slurp "src/ludus/postlude.ld")
   		:cljs (r/inline "./postlude.ld")
   		))