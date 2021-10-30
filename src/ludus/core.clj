(ns ludus.core
  "A tree-walk interpreter for the Ludus language."
  (:require
   [ludus.scanner :as scanner]))

(defn- report [line, where, message]
  (println (str "[line " line "] Error" where ": " message)))

(defn- error [line, message]
  (report line "" message))

(defn- run [source]
  (let [tokens (scanner/scan source)]
    (run! println tokens)))

(defn- run-file [path]
  (let [source (slurp path)]
    (run source)))

(defn- run-prompt []
  (loop [_ ""]
    (print "Ludus >> ")
    (flush)
    (when-let [line (read-line)]
      (recur (run line)))))

(defn -main [& args]
  (cond
    (> (count args) 1) (do
                         (println "Usage: ludus [script]")
                         (System/exit 64))
    (= (count args) 1) (run-file (first args))
    :else (run-prompt)))