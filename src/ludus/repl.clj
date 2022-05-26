(ns ludus.repl
  (:require 
    [ludus.scanner :as scanner]
    [ludus.parser :as parser]
    [ludus.interpreter :as interpreter]
    [ludus.prelude :as prelude]
    [ludus.show :as show]))

(def sessions (atom {}))

(def current-session (atom nil))

(def prompt "=> ")

(defn- new-session [name] 
  (let [session (atom {
                       :name name 
                       :ctx (volatile! (merge prelude/prelude {::repl true})) 
                       :history []})]
    (swap! sessions #(assoc % name session))
    session))

(defn repl-loop [session-atom]
  (let [session @session-atom]
    (print (str (:name session) prompt))
    (flush)
    (let [input (read-line)
          parsed (-> input (scanner/scan) (parser/parse))
          {result :result ctx :ctx} (interpreter/interpret-repl parsed (:ctx session))]
      (if (= result ::interpreter/error)
        (repl-loop session-atom)
        (do
          (println (show/show result))
          (swap! session-atom #(assoc % :ctx ctx))
          (repl-loop session-atom))))))

(defn launch []
  (println "Welcome to Ludus (v. 0.1.0-alpha)")
  (let [session (new-session "ludus")]
    (reset! current-session session)
    (repl-loop session)))

