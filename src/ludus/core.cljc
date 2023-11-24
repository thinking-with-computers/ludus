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
            (show/show interpreted)))))))