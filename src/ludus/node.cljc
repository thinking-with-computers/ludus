(ns ludus.node
 	(:require [ludus.interpreter :as i]
  	 [ludus.grammar :as g]
  	 [ludus.parser :as p]
  	 [ludus.scanner :as s]
  	 [ludus.prelude :as pre]
  	 [ludus.show :as show]
  	 [ludus.base :as base]
  	 [ludus.data :as data]
  	 )
 	)

(declare ld->clj)

(defn cljify [[k v]] [k (ld->clj v)])

(defn ld->clj [value]
 	(case (base/get-type value)
  		(:nil :number :string :boolean :keyword :set) value

  		(:list :tuple) (into [] (map ld->clj) (rest value))

  		(:dict :struct :ns) (into {} (map cljify) (dissoc value ::data/dict ::data/struct ::data/type ::data/name))

  		:ref (ld->clj @(::value value))

  		:fn (throw (ex-info (str "Cannot export functions from Ludus to Clojure. You tried exporting " (show/show value)) {}))))

(defn run [source]
 	(let [user_scanned (s/scan source)
      		user_tokens (:tokens user_scanned)
      		user_parsed (p/apply-parser g/script user_tokens)
      		user_result (i/interpret-safe source user_parsed {})
      		post_scanned (s/scan pre/postlude)
      		post_tokens (:tokens post_scanned)
      		post_parsed (p/apply-parser g/script post_tokens)
      		post_result (i/interpret-safe source post_parsed {})
      		ludus_result (assoc post_result :result user_result)
      		clj_result (ld->clj ludus_result)
      		]
   	#?(:clj clj_result :cljs (clj->js clj_result))
   	))