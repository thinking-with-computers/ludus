(ns ludus.core
  "A tree-walk interpreter for the Ludus language."
  (:require
    [ludus.scanner :as scanner]
    [ludus.parser :as parser]
    [ludus.interpreter :as interpreter]
    [ludus.show :as show]
    [clojure.pprint :as pp]))

(defn- run [source]
  (let [scanned (scanner/scan source)]
    (if (not-empty (:errors scanned))
      (do
        (println "I found some scanning errors!")
        (pp/pprint (:errors scanned))
        (System/exit 65))
      (let [parsed (parser/parse scanned)]
        (if (not-empty (:errors parsed))
          (do
            (println "I found some parsing errors!")
            (pp/pprint (:errors parsed))
            (System/exit 66))
          (let [interpreted (interpreter/interpret parsed)]
            (println (show/show interpreted))
            (System/exit 0)))))))

(defn -main [& args]
  (cond
    (= (count args) 1) (run (slurp (first args)))
    :else (do
            (println "Usage: ludus [script]")
            (System/exit 64))))