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