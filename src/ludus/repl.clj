(ns ludus.repl
  (:require
    [ludus.scanner :as scanner]
    [ludus.parser :as p]
    [ludus.grammar :as g]
    [ludus.interpreter :as interpreter]
    [ludus.base :as base]
    [ludus.show :as show]
    [ludus.data :as data]
    ;[ludus.process :as process]
    ))

(declare repl-prelude new-session)

(def sessions (atom {}))

(def current-session (atom nil))

(def prompt "=> ")

(defn- exit []
  (println "\nGoodbye!")
  (System/exit 0))

(def repl-ctx (merge interpreter/ludus-prelude
                {::repl true
                 "repl"
                 {::data/struct true
                  ::data/type ::data/ns
                  ::data/name "repl"

                  :flush
                  {:name "flush"
                   ::data/type ::data/clj
                   :body (fn 
                           ([]
                            (let [session @current-session]
                              (swap! session #(assoc % :ctx (volatile! repl-ctx)))
                              :ok))
                           ([name]
                            (if-let [session (get @sessions name)]
                              (do
                                (swap! session #(assoc % :ctx (volatile! repl-ctx)))
                                :ok)
                              (do 
                                (println "No session named" name)
                                :error))))}

                  :new
                  {:name "new"
                   ::data/type ::data/clj
                   :body (fn [name]
                           (let [session (new-session name)]
                             (reset! current-session session)
                             :ok))}

                  :switch
                  {:name "switch"
                   ::data/type ::data/clj
                   :body (fn [name]
                           (if-let [session (get @sessions name)]
                             (do
                               (reset! current-session session)
                               :ok)
                             (do
                               (println "No session named" name)
                               :error)))}

                  :quit
                  {:name "quit"
                   ::data/type ::data/clj
                   :body (fn [] (exit))}
                  }}))

(defn- new-session [name]
  (let [session (atom {:name name
                       :ctx (volatile! repl-ctx)
                       :history []})]
    (swap! sessions #(assoc % name session))
    session))

(defn repl-loop []
  (let [session-atom @current-session
        session @session-atom
        orig-ctx (:ctx session)]
    (print (str (:name session) prompt))
    (flush)
    (let [input (read-line)]
      (cond
        (= nil input) (exit)

        (= "" input) (recur)

        :else
        (let [parsed (->> input 
                       (scanner/scan) 
                       :tokens 
                       (p/apply-parser g/script))]
          (if (= :err (:status parsed))
            (do
              (println (p/err-msg parsed))
              (recur))
            (let [{result :result ctx :ctx}
                  (interpreter/interpret-repl parsed orig-ctx)]
              (if (= result :error)
                (recur)
                (do
                  (println (show/show result))
                  (when (not (= @ctx @orig-ctx))
                    (swap! session-atom #(assoc % :ctx ctx)))
                  (recur))))))))))

(defn launch []
  (println "Welcome to Ludus (v. 0.1.0-alpha)")
  (let [session (new-session :ludus)]
    (reset! current-session session)
    (repl-loop)))

