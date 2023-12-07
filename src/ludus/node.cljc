(ns ludus.node
 	(:require [ludus.interpreter :as i]
  	 [ludus.grammar :as g]
  	 [ludus.parser :as p]
  	 [ludus.scanner :as s]
  	 [ludus.prelude :as pre]
  	 [ludus.show :as show]
  	 [ludus.base :as base]
  	 [ludus.data :as data]
  	 [ludus.error :as error]
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

(defn clean-out [value]
 	#?(:clj value :cljs (clj->js value)))

(defn run [source]
 	(let [user_scanned (s/scan source)
      		user_tokens (:tokens user_scanned)
      		user_parsed (p/apply-parser g/script user_tokens)
      		user_result (i/interpret-safe source user_parsed {})
      		result_str (show/show user_result)
      		post_scanned (s/scan pre/postlude)
      		post_tokens (:tokens post_scanned)
      		post_parsed (p/apply-parser g/script post_tokens)
      		post_result (i/interpret-safe source post_parsed {})
      		ludus_result (assoc post_result :result result_str)
      		clj_result (ld->clj ludus_result)
      		]
    (cond
     	(not-empty (:errors user_tokens))
     	(clean-out {:errors (:errors user_tokens)})

     	(= :err (:status user_parsed))
     	(clean-out {:errors [(error/parse-error source user_parsed)]})

     	(::data/error user_result)
     	(clean-out {:errors [user_result]})

     	:else
     	(clean-out clj_result)
     	)
   	))

(do

  (-> "foo" run :errors)
 	)
