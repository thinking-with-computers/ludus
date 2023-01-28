(ns ludus.process
	(:require
		[ludus.data :as data])
  (:import (java.util.concurrent Executors)))

;; virtual thread patch from https://ales.rocks/notes-on-virtual-threads-and-clojure
(defn- thread-factory [name]
  (-> (Thread/ofVirtual)
    (.name name 0)
    (.factory)))

(set-agent-send-off-executor!
  (Executors/newThreadPerTaskExecutor
    (thread-factory "ludus-vthread-")))

(def processes (atom {}))

(def current-pid (atom 1001))

(defn new-process []
  (let [pid @current-pid
        process (atom {:pid pid
                         :queue clojure.lang.PersistentQueue/EMPTY
                         :inbox nil
                         :status :occupied
                         })]
      (swap! processes #(assoc % pid process))
      (swap! current-pid inc)
      process))

(def vm-state (atom :stopped))

(defn- values [m] (into [] (map (fn [[_ v]] v)) m))

(defn- map-values [m f] (into {} (map (fn [[k v]] [k (f v)])) m))

(defn process-msg [process]
  ;;(println "processing message" self)
  (let [q (:queue process)
    inbox (:inbox process)]
    (when (not (realized? inbox))
      ;;(println "delivering message in" self)
      (deliver inbox (peek q))
      (assoc process :queue (pop q) :inbox nil))))

(defn run-process [process-atom]
  (let [process @process-atom
    status (:status process)
    q (:queue process)
    inbox (:inbox process)]
    ;;(println "running process" self ":" (into [] q))
    (when (and (= status :idle) (not-empty q) inbox)
      (swap! process-atom process-msg))))

(defn start-vm []
  ;; (println "Starting Ludus VM")
  (when (= @vm-state :stopped)
    (future
      (reset! vm-state :running)
      (loop []
        (when (= @vm-state :running)
            (run! run-process (values @processes))
            (recur)
            ;; (println "Ludus VM shutting down")
            )))))

(defn stop-vm []
  (reset! vm-state :stopped)
  (reset! processes {})
  (reset! current-pid 1001)
  nil)

(def process {"process" {
	::data/struct true
	::data/type ::data/ns
	::data/name "process"

	:list {::data/type ::data/clj
			:name "list"
			:body (fn [] (into [] (keys @processes)))}

	:info {::data/type ::data/clj
			:name "info"
			:body (fn [pid]
				(let [process @(get @processes pid)
					queue (into [] (:queue process))]
					(assoc process :queue queue ::data/dict true)))}

  :flush {::data/type ::data/clj
      :name "flush"
      :body (fn [pid]
        (let [process (get @processes pid)
          queue (into [] (:queue @process))]
          (swap! process #(assoc % :queue clojure.lang.PersistentQueue/EMPTY))
          queue))}
	}})