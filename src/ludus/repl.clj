(ns ludus.repl
  (:require 
    [ludus.scanner :as scanner]
    [ludus.parser :as parser]
    [ludus.interpreter :as interpreter]
    [ludus.prelude :as prelude]
    [ludus.show :as show]
    [ludus.data :as data]))

(declare repl-prelude new-session)

(def sessions (atom {}))

(def current-session (atom nil))

(def prompt "=> ")

(def base-ctx (merge prelude/prelude 
	{::repl true "foo" :bar
	 "repl"
	 {
	::data/struct true
	::data/type ::data/ns
	::data/name "repl"

	:flush
	{:name "flush"
	 ::data/type ::data/clj
	 :body (fn [] 
	 	(let [session @current-session]
	 		(swap! session #(assoc % :ctx (volatile! base-ctx)))
	 		:ok))}

    :new
    {:name "new"
	 ::data/type ::data/clj
	 :body (fn [name]
	 	(let [session (new-session name)]
	 		(reset! current-session session)
	 		:ok))}

	:swap
	{:name "swap"
	::data/type ::data/clj
	:body (fn [name]
		(if-let [session (get @sessions name)]
			(do
				(reset! current-session session)
				:ok)
			(do
				(println "No session named" name)
				:error)))}
	}}))

(defn- new-session [name] 
  (let [session (atom {
                       :name name 
                       :ctx (volatile! base-ctx) 
                       :history []})]
    (swap! sessions #(assoc % name session))
    session))

(defn repl-loop []
  (let [session-atom @current-session
  	session @session-atom
  	orig-ctx (:ctx session)]
    (print (str (:name session) prompt))
    (flush)
    (let [input (read-line)
          parsed (-> input (scanner/scan) (parser/parse))
          {result :result ctx :ctx} (interpreter/interpret-repl parsed (:ctx session))]
      (if (= result ::interpreter/error)
        (repl-loop)
        (do
          (println (show/show result))
          (when (not (= @ctx @orig-ctx))
          	(swap! session-atom #(assoc % :ctx ctx)))
          (repl-loop))))))

(defn launch []
  (println "Welcome to Ludus (v. 0.1.0-alpha)")
  (let [session (new-session "ludus")]
    (reset! current-session session)
    (repl-loop)))

