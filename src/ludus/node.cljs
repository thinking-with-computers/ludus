(ns ludus.node
 	(:require [ludus.interpreter :as i]
  	 [ludus.grammar :as g]
  	 [ludus.parser :as p]
  	 [ludus.scanner :as s])
 	)

(defn run [source]
 	(let [scanned (s/scan source)
      		tokens (:tokens scanned)
      		parsed (p/apply-parser g/script tokens)
      		result (i/interpret-safe source parsed {})]	
   	(clj->js {:result result
            		:errors []
            		:console []
            		:draw []})))