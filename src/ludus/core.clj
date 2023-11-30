(ns ludus.core
  "A tree-walk interpreter for the Ludus language."
  (:require
    [ludus.scanner :as scanner]
    [ludus.parser :as p]
    [ludus.grammar :as g]
    [ludus.interpreter :as interpreter]
    [ludus.show :as show]
    [clojure.pprint :as pp]
    [ludus.loader :as loader]
    [ludus.repl :as repl])
  (:gen-class))

(defn- run [file source]
  (let [scanned (scanner/scan source)]
    (if (not-empty (:errors scanned))
      (do
        (println "I found some scanning errors!")
        (pp/pprint (:errors scanned))
        (System/exit 65))
      (let [parsed (p/apply-parser g/script (:tokens scanned))]
        (if (p/fail? parsed)
          (do
            (println "I found some parsing errors!")
            (println (p/err-msg parsed))
            (System/exit 66))
          (let [interpreted (interpreter/interpret source file parsed)]
            (println (show/show interpreted))
            (System/exit 0)))))))

(defn -main [& args]
  (cond
    (= (count args) 1)
    (let [file (first args)
          source (loader/load-import file)]
      (run file source))

    :else (repl/launch)))