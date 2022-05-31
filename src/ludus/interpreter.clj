(ns ludus.interpreter
  (:require
    [ludus.parser :as parser]
    [ludus.scanner :as scanner]
    [ludus.ast :as ast]
    [ludus.prelude :as prelude]
    [ludus.data :as data]
    [ludus.show :as show]
    [ludus.loader :as loader]
    [ludus.token :as token]
    [clojure.pprint :as pp]
    [clojure.set]))

(def ^:dynamic self 1001)

;; right now this is not very efficient:
;; it's got runtime checking
;; we should be able to do these checks statically
;; that's for later, tho
(defn- resolve-word [word ctx-vol]
  (let [ctx @ctx-vol]
    (if (contains? ctx word)
      (get ctx word)
      (if (contains? ctx ::parent)
        (recur word (::parent ctx))
        (throw (ex-info (str "Unbound name: " word) {}))))))

(declare interpret-ast match interpret)

(def processes (atom {}))

(def current-pid (atom 1001))

(defn new-process []
  (let [pid @current-pid
        process (atom {:pid pid
                         :queue clojure.lang.PersistentQueue/EMPTY
                         :in nil
                         :status :occupied
                         })]
      (swap! processes #(assoc % pid process))
      (swap! current-pid inc)
      process))

(def vm-state (atom :stopped))

(defn- values [m] (into [] (map (fn [[_ v]] v)) m))

(defn- process-msg [process]
  ;;(println "processing message" self)
  (let [q (:queue process)
    in (:in process)]
    (when (not (realized? in))
      ;;(println "delivering message in" self)
      (deliver in (peek q))
      (assoc process :queue (pop q) :in nil))))

(defn- run-process [process-atom]
  (let [process @process-atom
    status (:status process)
    q (:queue process)
    in (:in process)]
    ;;(println "running process" self ":" (into [] q))
    (when (and (= status :idle) (not-empty q) in)
      (swap! process-atom process-msg))))

(defn- start-vm []
  ;; (println "Starting Ludus VM")
  (when (= @vm-state :stopped)
    (future
      (reset! vm-state :running)
      (loop []
        (if (= @vm-state :running)
          (do
            (run! run-process (values @processes))
            (recur))
          ;; (println "Ludus VM shutting down")
          )))))

(defn- stop-vm []
  (reset! vm-state :stopped)
  (reset! processes {})
  (reset! current-pid 1001)
  nil)

(defn- match-tuple [pattern value ctx-vol]
  (cond
    (not (vector? value)) {:success false :reason "Could not match non-tuple value to tuple"}

    (not (= ::data/tuple (first value))) {:success false :reason "Could not match list to tuple"}

    (not (= (:length pattern) (dec (count value))))
    {:success false :reason "Cannot match tuples of different lengths"}

    (= 0 (:length pattern) (dec (count value))) {:success true :ctx {}}

    :else (let [members (:members pattern)]
            (loop [i (:length pattern)
                   ctx {}]
              (if (= 0 i)
                {:success true :ctx ctx}
                (let [match? (match (nth members (dec i)) (nth value i) ctx-vol)]
                  (if (:success match?)
                    (recur (dec i) (merge ctx (:ctx match?)))
                    {:success false :reason (str "Could not match " pattern " with " value)})))))))

(defn- match-list [pattern value ctx-vol]
  (cond
    (not (vector? value)) {:success false :reason "Could not match non-list value to list"}

    (= ::data/tuple (first value)) {:success false :reason "Could not match tuple value to list pattern"}

    ;; TODO: fix this with splats
    (not (= (count (:members pattern)) (count value)))
    {:success false :reason "Cannot match lists of different lengths"}

    (= 0 (count (:members pattern)) (count value)) {:success true :ctx {}}

    :else (let [members (:members pattern)]
            (loop [i (dec (count members))
                   ctx {}]
              (if (> 0 i)
                {:success true :ctx ctx}
                (let [match? (match (nth members i) (nth value i) ctx-vol)]
                  (if (:success match?)
                    (recur (dec i) (merge ctx (:ctx match?)))
                    {:success false :reason (str "Could not match " pattern " with " value)})))))))

(defn- match-dict [pattern value ctx-vol]
  (cond
    (not (map? value))
    {:success false :reason "Could not match non-dict value to dict pattern"}

    (not (::data/dict value))
    {:success false :reason "Cannot match non-dict data types a dict pattern"}

    :else
    (let [members (:members pattern)
          kws (keys members)]
      (loop [i (dec (count kws)) ctx {}]
        (if (> 0 i)
          {:success true :ctx ctx}
          (let [kw (nth kws i)]
            (if (contains? value kw)
              (let [match? (match (kw members) (kw value) ctx-vol)]
                (if (:success match?)
                  (recur (dec i) (merge ctx (:ctx match?)))
                  {:success false :reason (str "Could not match " pattern " with " value " at key " kw)}))
              {:success false :reason (str "Could not match " pattern " with " value " at key " kw)})))))))

(defn- match-struct [pattern value ctx-vol]
  (cond
    (not (map? value))
    {:success false :reason "Could not match non-struct value to struct pattern"}

    (not (::data/struct value))
    {:success false :reason "Cannot match non-struct data types a struct pattern"}

    :else
    (let [members (:members pattern)
          kws (keys members)]
      (loop [i (dec (count kws)) ctx {}]
        (if (> 0 i)
          {:success true :ctx ctx}
          (let [kw (nth kws i)]
            (if (contains? value kw)
              (let [match? (match (kw members) (kw value) ctx-vol)]
                (if (:success match?)
                  (recur (dec i) (merge ctx (:ctx match?)))
                  {:success false :reason (str "Could not match " pattern " with " value " at key " kw)}))
              {:success false :reason (str "Could not match " pattern " with " value " at key " kw)})))))))

(defn- match [pattern value ctx-vol]
  (let [ctx @ctx-vol]
    (case (::ast/type pattern)
      ::ast/placeholder {:success true :ctx {}}

      ::ast/atom
      (let [match-value (:value pattern)]
        (if (= match-value value)
          {:success true :ctx {}}
          {:success false
           :reason (str "No match: Could not match " match-value " with " value)}))

      ::ast/word
      (let [word (:word pattern)]
        (if (contains? ctx word)
          {:success false :reason (str "Name " word " is already bound")}
          {:success true :ctx {word value}}))

      ::ast/tuple (match-tuple pattern value ctx-vol)

      ::ast/list (match-list pattern value ctx-vol)

      ::ast/dict (match-dict pattern value ctx-vol)

      ::ast/struct (match-struct pattern value ctx-vol)

      (throw (ex-info "Unknown pattern on line " {:pattern pattern})))))

(defn- update-ctx [ctx new-ctx]
  (merge ctx new-ctx))

;; TODO: get "if let" pattern working
;; TODO: get typed exceptions to distinguish panics
(defn- interpret-let [ast ctx]
  (let [pattern (:pattern ast)
        expr (:expr ast)
        value (interpret-ast expr ctx)
        match (match pattern value ctx)
        success (:success match)]
    (if success
      (vswap! ctx update-ctx (:ctx match))
      (throw (ex-info (:reason match) {:ast ast})))
    value))

(defn- interpret-if [ast ctx]
  (let [if-expr (:if ast)
        then-expr (:then ast)
        else-expr (:else ast)
        if-value (interpret-ast if-expr ctx)]
    (if if-value
      (interpret-ast then-expr ctx)
      (interpret-ast else-expr ctx))))

(defn- interpret-match [ast ctx]
  (let [match-expr (:expr ast)
        expr (interpret-ast match-expr ctx)
        clauses (:clauses ast)]
    (loop [clause (first clauses)
           clauses (rest clauses)]
      (if clause
        (let [pattern (:pattern clause)
              body (:body clause)
              new-ctx (volatile! {::parent ctx})
              match? (match pattern expr new-ctx)
              success (:success match?)
              clause-ctx (:ctx match?)]
          (if success
            (do
              (vswap! new-ctx #(merge % clause-ctx))
              (interpret-ast body new-ctx))
            (recur (first clauses) (rest clauses))))
        (throw (ex-info "Match Error: No match found" {:ast ast}))))))

(defn- interpret-cond [ast ctx]
  (let [clauses (:clauses ast)]
    (loop [clause (first clauses)
           clauses (rest clauses)]
      (if (not clause)
        (throw (ex-info "Cond Error: No match found" {:ast ast}))
        (let [test-expr (:test clause)
              body (:body clause)
              truthy? (boolean (interpret-ast test-expr ctx))]
          (if truthy?
            (interpret-ast body ctx)
            (recur (first clauses) (rest clauses))))))))

(defn- interpret-called-kw [kw tuple ctx]
  ;; TODO: check this statically
  (if (not (= 1 (:length tuple)))
    (throw (ex-info "Called keywords must be unary" {:ast kw}))
    (let [kw (interpret-ast kw ctx)
          map (second (interpret-ast tuple ctx))]
      (if (::data/struct map)
        (if (contains? map kw)
          (kw map)
          (if (= (::data/type map) ::data/ns)
            (throw (ex-info (str "Namespace error: no member " kw " in ns " (::data/name map)) {:ast kw}))
            (throw (ex-info (str "Struct error: no member at " kw) {:ast kw}))))
        (get map kw)))))

(defn- call-fn [lfn tuple ctx]
  (cond
    (= ::data/partial (first tuple))
    {::data/type ::data/clj
     :name (str (:name lfn) "{partial}")
     :body (fn [arg]
             (call-fn
               lfn
               (concat [::data/tuple] (replace {::data/placeholder arg} (rest tuple)))
               ctx))}

    (= (::data/type lfn) ::data/clj) (apply (:body lfn) (next tuple))

    (= (::data/type lfn) ::data/fn)
    (let [clauses (:clauses lfn)
          closed-over (:ctx lfn)]
      (loop [clause (first clauses)
             clauses (rest clauses)]
        (if clause
          (let [pattern (:pattern clause)
                body (:body clause)
                fn-ctx (volatile! {::parent closed-over})
                match? (match pattern tuple fn-ctx)
                success (:success match?)
                clause-ctx (:ctx match?)]
            (if success
              (do
                (vswap! fn-ctx #(merge % clause-ctx))
                (interpret-ast body fn-ctx))
              (recur (first clauses) (rest clauses))))

          (throw (ex-info "Match Error: No match found" {:ast (:ast lfn)})))))

    (keyword? lfn)
    (if (= 2 (count tuple))
      (let [target (second tuple) kw lfn]
        (if (::data/struct target)
          (if (contains? target kw)
            (kw target)
            (if (= (::data/type target) ::data/ns)
              (throw (ex-info (str "Namespace error: no member " kw " in ns" (::data/name target)) {:ast kw}))
              (throw (ex-info (str "Struct error: no member at " kw) {:ast kw}))))

          (kw target)))
      (throw (ex-info "Called keywords take a single argument" {:ast lfn})))

    :else (throw (ex-info "I don't know how to call that" {:ast lfn}))))

(defn- interpret-synthetic-term [prev-value curr ctx]
  (let [type (::ast/type curr)]
    (if (= type ::ast/atom)
      (if (::data/struct prev-value)
        (if (contains? prev-value (:value curr))
          (get prev-value (:value curr))
          (if (= (::data/type prev-value) ::data/ns)
            (throw (ex-info (str "Namespace error: no member " (:value curr) " in ns " (::data/name prev-value)) {:ast curr}))
            (throw (ex-info (str "Struct error: no member " (:value curr)) {:ast curr}))))
        (get prev-value (:value curr)))
      (call-fn prev-value (interpret-ast curr ctx) ctx))))

(defn- interpret-synthetic [ast ctx]
  (let [terms (:terms ast)
        first (first terms)
        second (second terms)
        rest (rest (rest terms))
        first-term-type (::ast/type first)
        first-val (if (= first-term-type ::ast/atom)
                    (interpret-called-kw first second ctx)
                    (interpret-synthetic-term (interpret-ast first ctx) second ctx))]
    (reduce #(interpret-synthetic-term %1 %2 ctx) first-val rest)))

(defn- interpret-fn [ast ctx] ;; TODO: fix context/closure (no cycles)?
  (let [name (:name ast)
        clauses (:clauses ast)]
    (if (= name ::ast/anon)
      {::data/type ::data/fn
       :name name
       :ast ast
       :clauses clauses
       :ctx ctx}
      (let [fn {::data/type ::data/fn
                :name name
                :clauses clauses
                :ctx ctx}]
        (if (contains? @ctx name)
          (throw (ex-info (str "Name " name " is already bound") {:ast ast}))
          (do
            (vswap! ctx update-ctx {name fn})
            fn))))))

(defn- interpret-do [ast ctx]
  (let [exprs (:exprs ast)
        origin (interpret-ast (first exprs) ctx)
        fns (rest exprs)]
    (reduce #(call-fn (interpret-ast %2 ctx) [::data/tuple %1] ctx) origin fns)))

(defn- map-values [f]
  (map (fn [kv]
         (let [[k v] kv]
           [k (f v)]))))

(defn- interpret-ns [ast ctx]
  (let [members (:members ast)
        name (:name ast)]
    (if (contains? @ctx name)
      (throw (ex-info (str "ns name " name " is already bound") {:ast ast}))
      (let [ns (into
                 {::data/struct true ::data/type ::data/ns ::data/name name}
                 (map-values #(interpret-ast % ctx))
                 members)]
        (vswap! ctx update-ctx {name ns})
        ns))))

(defn- interpret-import [ast ctx]
  (let [path (:path ast)
        name (:name ast)]
    (if (contains? @ctx name)
      (throw (ex-info (str "Name " name " is alrady bound") {:ast ast}))
      (let [source (try
                     (loader/load-import path (resolve-word ::file ctx))
                     (catch Exception e
                       (if (::loader/error (ex-data e))
                         (throw (ex-info (ex-message e) {:ast ast}))
                         (throw e))))
            result (-> source (scanner/scan) (parser/parse) (interpret path))]
        (vswap! ctx update-ctx {name result})
        result ;; TODO: test this!
        ))))

(defn- interpret-ref [ast ctx]
  (let [name (:name ast) expr (:expr ast)]
    (when (contains? @ctx name)
      (throw (ex-info (str "Name " name " is already bound") {:ast ast})))
    (let [value (interpret-ast expr ctx)
          box (atom value)
          ref {::data/ref true ::data/value box ::data/name name}]
      (vswap! ctx update-ctx {name ref})
      ref)))

(defn- interpret-loop [ast ctx]
  (let [tuple (interpret-ast (:expr ast) ctx)
        clauses (:clauses ast)]
    (loop [input tuple]
      (let [output (loop [clause (first clauses)
                          clauses (rest clauses)]
                     (if clause
                       (let [pattern (:pattern clause)
                             body (:body clause)
                             new-ctx (volatile! {::parent ctx})
                             match? (match pattern input new-ctx)
                             success (:success match?)
                             clause-ctx (:ctx match?)]
                         (if success
                           (do
                             (vswap! new-ctx #(merge % clause-ctx))
                             (interpret-ast body new-ctx))
                           (recur (first clauses) (rest clauses))))

                       (throw (ex-info (str "Match Error: No match found in loop for " input) {:ast ast}))))]
        (if (::data/recur output)
          (recur (:tuple output))
          output)))))

(defn- panic [ast ctx]
  (throw (ex-info (show/show (interpret-ast (:expr ast) ctx)) {:ast ast})))

(defn- list-term [ctx]
  (fn [list member]
    (if (= (::ast/type member) ::ast/splat)
      (let [splatted (interpret-ast (:expr member) ctx)
            splat-list? (and
                          (vector? splatted)
                          (not (= (first splatted) ::data/tuple)))]
        (if splat-list?
          (concat list splatted)
          (throw (ex-info "Cannot splat non-list into list" {:ast member}))))
      (concat list [(interpret-ast member ctx)]))))

(defn- interpret-list [ast ctx]
  (let [members (:members ast)]
    (into [] (reduce (list-term ctx) [] members))))

(defn- set-term [ctx]
  (fn [set member]
    (if (= (::ast/type member) ::ast/splat)
      (let [splatted (interpret-ast (:expr member) ctx)
            splat-set? (set? splatted)]
        (if splat-set?
          (clojure.set/union set splatted)
          (throw (ex-info "Cannot splat non-set into set" {:ast member}))))
      (conj set (interpret-ast member ctx)))))

(defn- interpret-set [ast ctx]
  (let [members (:members ast)]
    (reduce (set-term ctx) #{} members)))

(defn- dict-term [ctx]
  (fn [dict member]
    (if (= (::ast/type member) ::ast/splat)
      (let [splatted (interpret-ast (:expr member) ctx)
            splat-map? (and
                         (map? splatted)
                         (::data/dict splatted))]
        (if splat-map?
          (merge dict splatted)
          (throw (ex-info "Cannot splat non-dict into dict" {:ast member}))))
      (let [k (first member) v (second member)]
        (assoc dict k (interpret-ast v ctx))))))

(defn- interpret-dict [ast ctx]
  (let [members (:members ast)]
    (assoc (reduce (dict-term ctx) {} members) ::data/dict true)))

(defn- interpret-receive [ast ctx]
  (let [process-atom (get @processes self)
    in (promise)
    clauses (:clauses ast)]
    ;; (println "receiving in" self)
    (swap! process-atom #(assoc % :in in :status :idle))
    (let [msg @in]
      (swap! process-atom #(assoc % :status :occupied))
      ;; (println "message received by" self ":" msg)
      (loop [clause (first clauses)
           clauses (rest clauses)]
      (if clause
        (let [pattern (:pattern clause)
              body (:body clause)
              new-ctx (volatile! {::parent ctx})
              match? (match pattern msg new-ctx)
              success (:success match?)
              clause-ctx (:ctx match?)]
          (if success
            (do
              (vswap! new-ctx #(merge % clause-ctx))
              (let [result (interpret-ast body new-ctx)]
                (swap! process-atom #(assoc % :status :idle))
                result))
            (recur (first clauses) (rest clauses))))
        (throw (ex-info "Match Error: No match found" {:ast ast})))))))

(defn- interpret-send [ast ctx]
  (let [msg (interpret-ast (:msg ast) ctx) 
    pid (interpret-ast (:pid ast) ctx)
    process-atom (get @processes pid)
    process @process-atom
    q (:queue process)
    status (:status process)]
    (when (not (= :dead status))
      (swap! process-atom #(assoc % :queue (conj q msg)))
      ;;(println "sent" msg "to" (:pid process))
        )
    msg))

(defn- interpret-spawn [ast ctx]
  (let [expr (:expr ast)
    process (new-process)
    pid (:pid @process)]
    (with-bindings {#'self pid}
      (future
        (try (interpret-ast expr ctx)
          (catch Exception e
          (println "Panic in Ludus process" (str self ":") (ex-message e))))
        (swap! @process #(assoc % :status :dead))))
    pid))

(defn interpret-ast [ast ctx]
  (case (::ast/type ast)
    ::ast/self self

    ::ast/atom (:value ast)

    ::ast/word (resolve-word (:word ast) ctx)

    ::ast/let (interpret-let ast ctx)

    ::ast/if (interpret-if ast ctx)

    ::ast/match (interpret-match ast ctx)

    ::ast/cond (interpret-cond ast ctx)

    ::ast/synthetic (interpret-synthetic ast ctx)

    ::ast/fn (interpret-fn ast ctx)

    ::ast/pipeline (interpret-do ast ctx)

    ::ast/placeholder ::data/placeholder

    ::ast/ns (interpret-ns ast ctx)

    ::ast/import (interpret-import ast ctx)

    ::ast/ref (interpret-ref ast ctx)

    ::ast/panic (panic ast ctx)

    ::ast/spawn (interpret-spawn ast ctx)

    ::ast/send (interpret-send ast ctx)

    ::ast/receive (interpret-receive ast ctx)

    ::ast/recur
    {::data/recur true :tuple (interpret-ast (:tuple ast) ctx)}

    ::ast/loop (interpret-loop ast ctx)

    ::ast/block
    (let [exprs (:exprs ast)
          inner (pop exprs)
          last (peek exprs)
          ctx (volatile! {::parent ctx})]
      (run! #(interpret-ast % ctx) inner)
      (interpret-ast last ctx))

    ::ast/script
    (let [exprs (:exprs ast)
          inner (pop exprs)
          last (peek exprs)]
      (run! #(interpret-ast % ctx) inner)
      (interpret-ast last ctx))

    ;; note that, excepting tuples and structs,
    ;; runtime representations are bare
    ;; tuples are vectors with a special first member
    ::ast/tuple
    (let [members (:members ast)]
      (into
        [(if (:partial ast) ::data/partial ::data/tuple)]
        (map #(interpret-ast % ctx)) members))

    ::ast/list (interpret-list ast ctx)

    ::ast/set
    (interpret-set ast ctx)

    ::ast/dict (interpret-dict ast ctx)

    ::ast/struct
    (let [members (:members ast)]
      (into {::data/struct true} (map-values #(interpret-ast % ctx)) members))

    (throw (ex-info "Unknown AST node type" {:ast ast}))))

(defn interpret [parsed file]
  (try
    (let [base-ctx (volatile! (merge {:file file} prelude/prelude))
      process (new-process)]
      (start-vm)
      (with-bindings {#'self (:pid @process)}
        (let [result (interpret-ast (::parser/ast parsed) base-ctx)]
          (swap! process #(assoc % :status :dead))
          (stop-vm)
          result)))
    (catch clojure.lang.ExceptionInfo e
      (println "Ludus panicked in" file)
      (println "On line" (get-in (ex-data e) [:ast :token ::token/line]))
      (println (ex-message e))
      (System/exit 67))))

(defn interpret-safe [parsed]
  (try
    (let [base-ctx (volatile! (merge {} prelude/prelude))
      process (new-process)]
      (start-vm)
      (with-bindings {#'self (:pid @process)}
        (let [result (interpret-ast (::parser/ast parsed) base-ctx)]
          (swap! process #(assoc % :status :dead))
          result))
      (stop-vm))
    (catch clojure.lang.ExceptionInfo e
      (stop-vm)
      (println "Ludus panicked!")
      (println "On line" (get-in (ex-data e) [:ast :token ::token/line]))
      (println (ex-message e))
      (pp/pprint (ex-data e)))))

(defn interpret-repl
  ([parsed]
   (let [base-ctx (volatile! (merge {} prelude/prelude))]
     (try
       (let [result (interpret-ast (::parser/ast parsed) base-ctx)]
         {:result result :ctx base-ctx})
       (catch clojure.lang.ExceptionInfo e
         (println "Ludus panicked!")
         (println (ex-message e))
         {:result ::error :ctx base-ctx}))))
  ([parsed ctx]
   (let [orig-ctx @ctx]
     (try
       (let [result (interpret-ast (::parser/ast parsed) ctx)]
         {:result result :ctx ctx})
       (catch clojure.lang.ExceptionInfo e
         (println "Ludus panicked!")
         (println (ex-message e))
         {:result ::error :ctx (volatile! orig-ctx)})))))


(comment
  (start-vm)
  (def source "
    fn echo () -> {
      & print (self, :echoing)
      loop () with () -> {
          & print (self, :looping)
          receive {
            msg -> {
              print (\"from \", self, \": \", msg)
              recur ()
            }
          }
        }
      }

    let echoer = spawn echo ()
    
    send :hello to echoer
    send :foo to echoer
    send :bar to echoer
    send :baz to echoer

    sleep (1000)

    ")

  (println "")
  (println "****************************************")
  (println "*** *** NEW INTERPRETATION *** ***")
  (println "")

  (let [result (-> source
      (scanner/scan)
      (parser/parse)
      (interpret-safe)
      (show/show)
      )]
  result))

(comment "

	Left to do:
	* if-let pattern
	* improve panics
	* add location info for panics
  * refactor calling keywords
  * refactor accessing structs vs. hashes

  ")





