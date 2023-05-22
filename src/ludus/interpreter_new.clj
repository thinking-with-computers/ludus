(ns ludus.interpreter-new
 	(:require
  		[ludus.grammar :as g]
  		[ludus.parser-new :as p]
  		[ludus.scanner :as s]))

(def source 
  "spawn foo
"
  )

(def tokens (-> source s/scan :tokens))

(def result (p/apply-parser g/spawn tokens))	

(-> result :data) 

(defn report [node]	
 	(when (p/fail? node) (p/err-msg node))	
 	node)	

(defn clean [node]	
 	(if (map? node)	
  		(-> node	
   			(report)	
   			(dissoc 	
    				:status 	
    				:remaining 	
    				:token)	
   			(update :data #(into [] (map clean) %)))	
  		node))	

(defn tap [x] (println "\n\n\n\n******NEW RUN\n\n:::=> " x "\n\n") x)	

(def my-data (-> result 	
              	clean 	
              	tap	
              	))
