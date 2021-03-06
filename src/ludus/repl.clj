(ns ludus.repl
  (:require
    [ludus.scanner :as scanner]
    [ludus.parser :as parser]
    [ludus.interpreter :as interpreter]
    [ludus.prelude :as prelude]
    [ludus.show :as show]
    [ludus.data :as data]
    [ludus.process :as process]))

(declare repl-prelude new-session)

(def sessions (atom {}))

(def current-session (atom nil))

(def prompt "=> ")

(defn- exit []
  (println "\nGoodbye!")
  (System/exit 0))

(def base-ctx (merge prelude/prelude process/process
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
                              (swap! session #(assoc % :ctx (volatile! base-ctx)))
                              :ok))
                           ([name]
                            (if-let [session (get @sessions name)]
                              (do
                                (swap! session #(assoc % :ctx (volatile! base-ctx)))
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
                       :ctx (volatile! base-ctx)
                       :history []})]
    (swap! sessions #(assoc % name session))
    session))

(defn repl-loop []
  (let [session-atom @current-session
        session @session-atom
        orig-ctx (:ctx session)
        pid (:pid session)]
    (print (str (:name session) prompt))
    (flush)
    (let [input (read-line)]
      (cond
        (= nil input) (exit)

        (= "" input) (recur)

        :else
        (let [parsed (-> input (scanner/scan) (parser/parse))
              {result :result ctx :ctx pid- :pid}
              (if pid
                (interpreter/interpret-repl parsed orig-ctx pid)
                (interpreter/interpret-repl parsed orig-ctx))]
          (if (= result ::interpreter/error)
            (recur)
            (do
              (println (show/show result))
              (when (not (= @ctx @orig-ctx))
                (swap! session-atom #(assoc % :ctx ctx)))
              (when (not (= pid pid-))
                (swap! session-atom #(assoc % :pid pid-)))
              (recur))))))))

(defn launch []
  (println "Welcome to Ludus (v. 0.1.0-alpha)")
  (let [session (new-session :ludus)]
    (reset! current-session session)
    (repl-loop)))

