(ns ludus.core
 	(:require
  		[ludus.scanner :as scanner]
  		[ludus.parser :as parser]
  		[ludus.grammar :as grammar]
  		[ludus.interpreter :as interpreter]
  		[ludus.show :as show]
  		))

(defn run [source]
  (println (str "Running some ludus source: " source))
  (let [scanned (scanner/scan source)]
    (if (not-empty (:errors scanned))
      (do
        (println "I found some scanning errors!")
        (println (:errors scanned))
        )
      (let [parsed (parser/apply-parser grammar/script (:tokens scanned))]
        (println "Scanned: ")
        (println scanned)
        (if (parser/fail? parsed)
          (do
            (println "I found some parsing errors!")
            (println (parser/err-msg parsed))
            nil
            )
          (let [interpreted (interpreter/interpret source parsed)]
            (println (show/show interpreted))
            interpreted))))))

(defn main! []
  (println "Ludus says, hi there...")
  
  #?(:clj (println "...from Clojure.")
     :cljs (println "...from ClojureScript."))
  
  (run ":foo")
  (run "add (1, 2)")
  (run "nil")
  (run "if true then :foo else :bar")
  )

(run ":foo")
(run "add (1, 2)")
(run "nil")
(run "if true then :foo else :bar")