(ns ludus.interpreter
  (:require
    [ludus.parser :as p]
    [ludus.grammar :as g]
    [ludus.scanner :as scanner]
    [ludus.ast :as ast]
    [ludus.base :as base]
    [ludus.prelude :as prelude]
    [ludus.data :as data]
    [ludus.show :as show]
    ;;[ludus.loader :as loader]
    [clojure.pprint :as pp]
    [clojure.set]
    [clojure.string]))

(defn prettify-ast [ast]
  (cond
    (not (map? ast)) ast
    (not (:data ast)) (dissoc ast :remaining :token)
    :else (let [{:keys [type data]} ast]
            {:type type ;:token token
             :data (into [] (map prettify-ast) data)})
    ))

;; right now this is not very efficient:
;; it's got runtime checking
;; we should be able to do these checks statically
;; that's for later, tho
(defn- ludus-resolve [key ctx-vol]
  (let [ctx @ctx-vol]
    ;(println "Resolving " key " in context " (keys ctx))
    ;(println "Current context: " (keys ctx))
    ;(println "Parent context: " (keys (if (::parent ctx) (deref (::parent ctx)) {})))
    (if (contains? ctx key)
      (get ctx key)
      (if (contains? ctx ::parent)
        (recur key (::parent ctx))
        ::not-found))))

(defn- resolve-word [word ctx]
  (let [value (ludus-resolve (-> word :data first) ctx)]
    (if (= ::not-found value)
      (throw (ex-info (str "Unbound name: " (-> word :data first)) {:ast word}))
      value)))

(declare interpret-ast match interpret interpret-file)

(defn- match-splatted [pattern value ctx-vol]
  (let [members (:data pattern)
        non-splat (pop members)
        splattern (peek members)
        length (count members)
        ctx-diff (volatile! @ctx-vol)]
    (if (> length (-> value count dec))
      {:success false :reason "Could not match different lengths"}
      (loop [i 0]
        (if (= (dec length) i)
          (let [last-binding (-> splattern :data first)
                binding-type (:type last-binding)]
            (if (= binding-type :word)
              (let [splat-ctx (:ctx (match
                                      last-binding
                                      (into [::data/list] (subvec value (inc i)))
                                      ctx-diff))] 
                {:success true :ctx (merge @ctx-diff splat-ctx)})
              {:success true :ctx @ctx-diff}))
          (let [match? (match (nth non-splat i) (nth value (inc i)) ctx-diff)]
            (if (:success match?)
              (do
                (vswap! ctx-diff #(merge % (:ctx match?)))
                ;(println "current context: " (dissoc @ctx-diff ::parent))
                (recur (inc i)))
              {:success :false :reason (str "Could not match " (show/show-pattern pattern) " with " (show/show value))}
              )))))))

;; Match-tuple is misbehaving when the first value is a function and the second is a list
;; Hangs on success!
;; printlns at top run just fine
;; println at top of :else - loop happens once
;; println in :else - loop - if - let binding match? does not happen
;; that suggets that match is hanging here

(defn- match-tuple [pattern value ctx-vol]
  ;(println "\n\n\n**********Matching tuple")
  ;(println "*****Value:   " (show/show value))
  ;(println "*****Pattern: " (show/show-pattern pattern))
  (let [members (:data pattern)
        length (count members)]
    (cond
      (not (vector? value)) {:success false :reason "Could not match non-tuple value to tuple"}
  
      (not (= ::data/tuple (first value))) {:success false :reason "Could not match list to tuple"}
  
      (= :splattern (:type (peek members)))
      (match-splatted pattern value ctx-vol)
  
      (not (= length (dec (count value))))
      {:success false :reason "Cannot match tuples of different lengths"}
  
      (= 0 length (dec (count value))) {:success true :ctx {}}
  
      :else 
      (let [ctx-diff (volatile! @ctx-vol)]
        (loop [i length]
          ;(println "Matching tuple elements at index " i)
          (if (= 0 i)
            {:success true :ctx @ctx-diff}
            (let [match? (match (nth members (dec i)) (nth value i) ctx-diff)]
              ;(println "Maybe a match?: " (dissoc match? :ctx))
              (if (:success match?)
                (do
                  (vswap! ctx-diff #(merge % (:ctx match?)))
                  (recur (dec i)))
                {:success false :reason (str "Could not match " (show/show-pattern pattern) " with " (show/show value) " because " (:reason match?))}))))))))

;; TODO: update this to use new AST representation
;; TODO: update this to reflect first element of list is ::data/list
(defn- match-list [pattern value ctx-vol]
  (let [members (:data pattern)
        splatted? (= :splattern (-> members peek :type))]
    (cond
      (not (vector? value)) 
      {:success false :reason (str "Could not match non-list value " (show/show value) " to list pattern " (show/show-pattern pattern))}
  
      (= ::data/tuple (first value)) 
      {:success false :reason (str "Could not match tuple value " (show/show value) " to list pattern " (show/show-pattern pattern))}

      splatted? 
      (match-splatted pattern value ctx-vol)
  
      ;; TODO: fix this with splats
      (not= (count members) (dec (count value)))
      {:success false :reason "Cannot match lists of different lengths"}
  
      (= 0 (count members) (dec (count value))) 
      {:success true :ctx {}}
  
      :else
      (let [ctx-diff (volatile! @ctx-vol)]
        (loop [i (dec (count members))]
          (if (> 0 i)
            {:success true :ctx @ctx-diff}
            (let [match? (match (nth members i) (nth value (inc i)) ctx-diff)]
              (if (:success match?)
                (do
                  (vswap! ctx-diff #(merge % (:ctx match?))) 
                  (recur (dec i)))
                {:success false :reason (str "Could not match " (show/show-pattern pattern) " with " (show/show value) " because " (:reason match?))}))))))))

(defn- member->kv [map member]
  (let [type (:type member)
        data (:data member)]  
    (case type
      :word
      (assoc map (keyword (first data)) member)

      :pair-pattern
      (assoc map (-> data first :data first) (second data))

      :typed
      (assoc map (-> data first :data first keyword) member)

      map ;;ignore splats
      )))

(defn- pattern-to-map [pattern]
  (let [members (:data pattern)]
    (reduce member->kv {} members)))

;; TODO: update this to match new AST representation
(defn- match-dict [pattern dict ctx-vol]
  (let [
        members (:data pattern)
        pattern-map (pattern-to-map pattern)
        kws (keys pattern-map)]
    ;(println "Matching with " pattern-map)
    (cond
      (not (map? dict))
      {:success false :reason (str "Could not match non-dict value " (show/show dict) " to dict pattern " (show/show-pattern pattern))}
  
      (not (::data/dict dict))
      {:success false :reason (str "Cannot match non-dict data types (ns, struct) " (show/show dict) " to a dict pattern " (show/show-pattern pattern))}

      (empty? members)
      {:success true :ctx {}}
  
      :else
      (let [ctx-diff (volatile! @ctx-vol)
            splat? (= :splattern (-> members peek :type))
            length (count kws)]
        (loop [i 0]
          (cond
            (> length i)
            (let [kw (nth kws i)
                  pattern-at (kw pattern-map)
                  value (kw dict)]
              (if (contains? dict kw)
                (let [match? (match pattern-at value ctx-diff)]
                  (if (:success match?)
                    (do
                      (vswap! ctx-diff #(merge % (:ctx match?)))
                      (recur (inc i)))
                    {:success false
                     :reason (str "Could not match " (show/show-pattern pattern) " with value " (show/show dict) " at key " kw " because " (:reason match?))}
                    ))
                {:success false
                 :reason (str "Could not match " (show/show-pattern pattern) " with " (show/show dict) " at key " kw " because there is no value at " kw)}))

            splat?
            (let [splat (-> members peek)
                  splat-data (-> splat :data first)
                  splat-type (-> splat-data :type)]
              (if (= :word splat-type)
                (let [unmatched (apply dissoc dict kws)
                      match? (match splat-data unmatched ctx-diff)]
                  (if (:success match?)
                    {:success true :ctx (merge @ctx-diff (:ctx match?))}
                    {:success false
                     :reason (str "Could not match " (show/show-pattern pattern) " with value " (show/show dict) " because " (:reason match?))}
                    ))
                {:success true :ctx @ctx-diff}
                ))

            :else
            {:success true :ctx @ctx-diff}

            ))))))

(defn- match-struct [pattern dict ctx-vol]
  (let [members (:data pattern)
        pattern-map (pattern-to-map pattern)
        kws (keys pattern-map)]
    (cond
      (not (map? dict))
      {:success false :reason (str "Could not match non-struct value " (show/show dict) " to struct pattern " (show/show-pattern pattern))}
  
      (not (::data/struct dict))
      {:success false :reason (str "Cannot match non-struct value " (show/show dict) " to struct pattern " (show/show-pattern pattern))}

      (empty? members)
      {:success true :ctx {}}
  
      :else
      (let [ctx-diff (volatile! @ctx-vol)
            splat? (= :splattern (-> members peek :type))
            length (count kws)]
        (loop [i 0]
          (cond
            (> length i)
            (let [kw (nth kws i)
                  pattern-at (kw pattern-map)
                  value (kw dict)]
              (if (contains? dict kw)
                (let [match? (match pattern-at value ctx-diff)]
                  (if (:success match?)
                    (do
                      (vswap! ctx-diff #(merge % (:ctx match?)))
                      (recur (inc i)))
                    {:success false
                     :reason (str "Could not match " (show/show-pattern pattern) " with value " (show/show dict) " at key " kw " because " (:reason match?))}
                    ))
                {:success false
                 :reason (str "Could not match " (show/show-pattern pattern) " with " (show/show dict) " at key " kw " because there is no value at " kw)}))

            splat?
            (let [splat (-> members peek)
                  splat-data (-> splat :data first)
                  splat-type (-> splat-data :type)]
              (if (= :word splat-type)
                (let [unmatched (assoc (apply dissoc dict ::data/struct kws) ::data/dict true)
                      match? (match splat-data unmatched ctx-diff)]
                  (if (:success match?)
                    {:success true :ctx (merge @ctx-diff (:ctx match?))}
                    {:success false
                     :reason (str "Could not match " (show/show-pattern pattern) " with value " (show/show dict) " because " (:reason match?))}
                    ))
                {:success true :ctx @ctx-diff}
                ))

            :else
            {:success true :ctx @ctx-diff}))))))

(defn- match-typed [pattern value ctx]
  (let [data (:data pattern)
        name (-> data first :data first)
        type (-> data second :data first)]
    (cond
      (contains? ctx name) {:success false :reason (str "Name " name "is already bound") :code :name-error}
      (not (= type (base/get-type value))) {:success false :reason (str "Could not match " (show/show-pattern pattern) " with " (show/show value) ", because types do not match")}
      :else {:success true :ctx {name value}})))

(defn- match [pattern value ctx-vol]
  ;(println "Matching " (show/show value) " with pattern " (show/show-pattern pattern))
  (let [ctx @ctx-vol]
    (case (:type pattern)
      (:placeholder :ignored :else)
      {:success true :ctx {}}

      (:number :nil :true :false :string :keyword)
      (let [match-value (-> pattern :data first)]
        (if (= match-value value)
          {:success true :ctx {}}
          {:success false
           :reason (str "No match: Could not match " (show/show-pattern match-value) " with " (show/show value))}))

      :word
      (let [word (-> pattern :data first)]
        (if (contains? ctx word)
          {:success false :reason (str "Name " word " is already bound") :code :name-error}
          {:success true :ctx {word value}}))

      :typed (match-typed pattern value ctx)

      :tuple-pattern (match-tuple pattern value ctx-vol)

      :list-pattern (match-list pattern value ctx-vol)

      :dict-pattern (match-dict pattern value ctx-vol)

      :struct-pattern (match-struct pattern value ctx-vol)

      (throw (ex-info (str "Unknown pattern type " (:type pattern)) {:ast pattern :value value})))))

(defn- update-ctx [ctx new-ctx]
  (merge ctx new-ctx))

(defn- interpret-let [ast ctx]
  (let [data (:data ast)
        pattern (first data)
        expr (second data)
        value (interpret-ast expr ctx)
        match (match pattern value ctx)
        success (:success match)]
    (if success
      (vswap! ctx update-ctx (:ctx match))
      (throw (ex-info (:reason match) {:ast ast})))
    value))

(defn- interpret-if-let [ast ctx]
 	(let [data (:data ast)
        if-ast (first data)
    		  then-expr (second data)
    		  else-expr (nth data 2)
        if-data (:data if-ast)
    		  let-pattern (first if-data)
    		  let-expr (second if-data)
    		  let-value (interpret-ast let-expr ctx)
    		  if-match (match let-pattern let-value ctx)
    		  success (:success if-match)]
  		(if success
   			(interpret-ast then-expr (volatile! (merge (:ctx if-match) {:parent ctx})))
   			(if (:code if-match)
    				(throw (ex-info (:reason if-match) {:ast if-ast}))
    				(interpret-ast else-expr ctx)))))

(defn- interpret-if [ast ctx]
  (let [data (:data ast)
        if-expr (first data)
        then-expr (second data)
        else-expr (nth data 2)]
    (if (= (:type if-expr) :let-expr)
      (interpret-if-let ast ctx)
      (if (interpret-ast if-expr ctx)
      	 (interpret-ast then-expr ctx)
      	 (interpret-ast else-expr ctx)))))

(defn- interpret-match [ast ctx]
  (let [data (:data ast)
        match-expr (first data)
        value (interpret-ast match-expr ctx)
        clauses (rest data)]
    (loop [clause (first clauses)
           clauses (rest clauses)]
      (if clause
        (let [clause-data (:data clause)
              pattern (first clause-data)
              guard (if (= 3 (count clause-data))
                      (second clause-data)
                      nil)
              body (peek clause-data)
              new-ctx (volatile! {::parent ctx})
              match? (match pattern value new-ctx)
              success (:success match?)
              clause-ctx (:ctx match?)]
          (if success
            (if guard 
              (if (interpret-ast guard (volatile! clause-ctx))
                (do
                  (vswap! new-ctx #(merge % clause-ctx))
                  (interpret-ast body new-ctx))
                (recur (first clauses) (rest clauses)))
              (do
                (vswap! new-ctx #(merge % clause-ctx))
                (interpret-ast body new-ctx)))
            (recur (first clauses) (rest clauses))))
        (throw (ex-info "Match Error: No match found" {:ast ast}))))))

(defn- interpret-cond [ast ctx]
  (let [clauses (:data ast)]
    (loop [clause (first clauses)
           clauses (rest clauses)]
      (if (not clause)
        (throw (ex-info "Cond Error: No match found" {:ast ast}))
        (let [data (:data clause)
              test-expr (first data)
              test-type (:type test-expr)
              body (second data)
              truthy? (or
                        (= :placeholder test-type)
                        (= :else test-type)
                        (interpret-ast test-expr ctx))]
          (if truthy?
            (interpret-ast body ctx)
            (recur (first clauses) (rest clauses))))))))

(defn- validate-args [args]
  (>= 1 (count (filter #(= :placeholder (:type %)) args))))

(defn- partial? [args]
  (some #(= :placeholder (:type %)) args))

(defn- interpret-called-kw [kw tuple ctx]
  (let [members (:data tuple)
        length (count members)]
    ;; TODO: check this statically
    (cond 
      (not (= 1 length)) 
      (throw (ex-info "Called keywords must be unary" {:ast tuple}))

      (partial? tuple) 
      (throw (ex-info "Called keywords may not be partially applied" {:ast tuple}))

      :else
      (let [kw (interpret-ast kw ctx)
            map (second (interpret-ast tuple ctx))]
        (if (::data/struct map)
          (if (contains? map kw)
            (kw map)
            (if (= (::data/type map) ::data/ns)
              (throw (ex-info (str "Namespace error: no member " kw " in ns " (::data/name map)) {:ast kw}))
              (throw (ex-info (str "Struct error: no member at " kw) {:ast kw}))))
          (get map kw))))))

(defn- call-fn [lfn args ctx]
  ;(println "Calling function " (:name lfn))
  (cond
    (= ::data/partial (first args))
    {::data/type ::data/clj
     :name (str (:name lfn) "{partial}")
     :body (fn [arg]
             (call-fn
               lfn
               (into [::data/tuple] (replace {::data/placeholder arg} (rest args)))
               ctx))}

    (= (::data/type lfn) ::data/clj) (apply (:body lfn) (next args))

    (= (::data/type lfn) ::data/fn)
    (let [clauses (:clauses lfn)
          closed-over (:ctx lfn)]
      (loop [clause (first clauses)
             clauses (rest clauses)]
        ;(println "Matching clause " clause)
        ;(println "With args " args)
        (if clause
          (let [pattern (first clause)
                guard (if (= 3 (count clause))
                        (second clause)
                        nil)
                body (peek clause)
                fn-ctx (volatile! {::parent closed-over})
                match? (match pattern args fn-ctx)
                success (:success match?)
                clause-ctx (:ctx match?)
                vclause (volatile! (assoc clause-ctx ::parent closed-over))]
            ;(println "Pattern: " pattern)
            ;(println "Body: " body)
            (if success
              (if guard 
                (if (do
                      ;(println "######### Testing guard")
                      ;(println "Context: " clause-ctx)
                      (interpret-ast guard vclause))
                  (do
                    ;(println "passed guard")
                    (vswap! fn-ctx #(merge % clause-ctx))
                    (interpret-ast body fn-ctx))
                  (recur (first clauses) (rest clauses)))
                (do
                  (vswap! fn-ctx #(merge % clause-ctx))
                  (interpret-ast body fn-ctx)))
              (recur (first clauses) (rest clauses))))

          (throw (ex-info (str "Match Error: No match found for " (show/show args) " in function " (:name lfn)) {:ast (:ast lfn)})))))

    (keyword? lfn)
    (if (= 2 (count args))
      (let [target (second args) kw lfn]
        (if (::data/struct target)
          (if (contains? target kw)
            (kw target)
            (if (= (::data/type target) ::data/ns)
              (throw (ex-info (str "Namespace error: no member " kw " in ns" (::data/name target)) {:ast kw}))
              (throw (ex-info (str "Struct error: no member at " kw) {:ast kw}))))

          (kw target)))
      (throw (ex-info "Called keywords take a single argument" {:ast lfn})))

    :else (throw (ex-info (str "I don't know how to call " (show/show lfn)) {:ast lfn}))))

(defn- interpret-args [args ctx]
  ;(println "interpreting arg" args)
  (if (partial? args)
    (if (validate-args args)
      (into [::data/partial] (map #(interpret-ast % ctx)) args) ; do the thing
      (throw (ex-info "Partially applied functions may only take a single argument" {:ast args})))
    (into [::data/tuple] (map #(interpret-ast % ctx)) args))
  )

(defn- interpret-synthetic-term [prev-value curr ctx]
  (let [type (:type curr)
        data (:data curr)]
    ;(println "interpreting synthetic type " type)
    ;(println "interpreting synthetic node " curr)
    (if (= type :keyword)
      (if (::data/struct prev-value)
        (if (contains? prev-value (first data))
          (get prev-value (first data))
          (if (= (::data/type prev-value) ::data/ns)
            (throw (ex-info (str "Namespace error: no member " (:value curr) " in ns " (::data/name prev-value)) {:ast curr}))
            (throw (ex-info (str "Struct error: no member " (:value curr)) {:ast curr}))))
        (get prev-value (first data)))
      (call-fn prev-value (interpret-args data ctx) ctx))))

(defn- interpret-synthetic [ast ctx]
  ;;(println "interpreting synthetic " ast)
  (let [data (:data ast)
        root (first data)
        terms (rest data)]
    ;(println "!!!!!!!!!Interpreting synthetic w/ root " (:data root))
    (if (seq terms)
      (do
        ;;(println "I've got terms!: " terms)
        (let [first-term (first terms)
              remaining (rest terms)
              first-val (if (= (:type root) :keyword)
                          (interpret-called-kw root first-term ctx)
                          (interpret-synthetic-term (interpret-ast root ctx) first-term ctx))]
          (reduce #(interpret-synthetic-term %1 %2 ctx) first-val remaining)))
      (interpret-ast root ctx))))

(defn- interpret-fn-inner [ast ctx] ;; TODO: fix context/closure (no cycles)?
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

(defn- build-fn
  ([ast ctx name clauses] (build-fn ast ctx name clauses nil))
  ([ast ctx name clauses docstring]
   (let [fnn {::data/type ::data/fn
              :name name
              :ast ast
              :clauses clauses
              :ctx ctx
              :doc docstring}]
     (if (= name :anon) 
       fnn
       (if (contains? @ctx name)
         (throw (ex-info (str "Name " name " is already bound") {:ast ast}))
         (do
           (vswap! ctx update-ctx {name fnn})
           fnn))))))

(defn- build-named-fn [ast ctx data]
  (let [name (-> data first :data first)
        body (-> data second)
        compound? (= :fn-compound (:type body))]
    (if compound?
      (if (= :string (-> body :data first :type))
        (build-fn ast ctx name (map :data (rest (:data body))) (-> body :data first :data first))
        (build-fn ast ctx name (map :data (:data body))))
      (build-fn ast ctx name [(:data body)]))))

(defn- interpret-fn [ast ctx]
  (let [data (:data ast)]
    (case (:type (first data))
      :fn-clause (build-fn ast ctx :anon [(-> data first :data)])
      :word (build-named-fn ast ctx data))))

(defn- interpret-do [ast ctx]
  (let [data (:data ast)
        root (interpret-ast (first data) ctx)
        fns (rest data)]
    (reduce #(call-fn (interpret-ast %2 ctx) [::data/tuple %1] ctx) root fns)))

(defn- map-values [f]
  (map (fn [kv]
         (let [[k v] kv]
           [k (f v)]))))

(defn- map-keys [f]
  (map (fn [[k v]] [(f k) v])))

; (defn- interpret-import [ast ctx]
;            (let [data (:data ast)
;                  path (-> data first :data first)
;                  name (-> data second :data first)
;                  file (ludus-resolve :file ctx)
;                  from (if (= ::not-found file) :cwd file)]
;              (if (contains? @ctx name)
;                (throw (ex-info (str "Name " name " is alrady bound") {:ast ast}))
;                (let [source (try
;                               (loader/load-import path from)
;                               (catch Exception e
;                                 (if (::loader/error (ex-data e))
;                                   (throw (ex-info (ex-message e) {:ast ast}))
;                                   (throw e))))
;                      parsed (->> source (scanner/scan) :tokens (p/apply-parser g/script))] 
;                  (if (p/fail? parsed)
;                    (throw (ex-info 
;                             (str "Parse error in file " path "\n"
;                               (p/err-msg parsed))
;                             {:ast ast}))        
;                    (let [interpret-result (interpret-file source path parsed)]
;                      (vswap! ctx update-ctx {name interpret-result})
;                      interpret-result))
;                  ))))

(defn- kw->str [kw] (apply str (rest (str kw))))

(defn- str->word [wordstr] {:type :word :data [wordstr]})

(defn- interpret-use [ast ctx]
  (let [data (:data ast)
        word (first data)
        ns (resolve-word word ctx)]
    ; (println "use: " ns)
    (if (not (= (::data/type ns) ::data/ns))
      (throw (ex-info (str "`use` may only use namespaces; " (-> word :data first) " is not a namespace") {:ast ast}))
      (let [ns-entries (dissoc ns ::data/type ::data/name ::data/struct)
            ns-keys (map kw->str (keys ns-entries))
            ns-words (into [] (map str->word) ns-keys)
            implied-pattern {:type :struct-pattern :data ns-words}
            implied-synthetic {:type :synthetic :data [word]}
            sugared-let {:type :let-expr :data [implied-pattern implied-synthetic]}]
        (interpret-let sugared-let ctx)
        )
      )
    ))

(defn- interpret-ref [ast ctx]
  (let [data (:data ast)
        name (-> data first :data first) 
        expr (-> data second)]
    (when (contains? @ctx name)
      (throw (ex-info (str "Name " name " is already bound") {:ast ast})))
    (let [value (interpret-ast expr ctx)
          box (atom value)
          ref {::data/ref true ::data/value box ::data/name name}]
      (vswap! ctx update-ctx {name ref})
      ref)))

(defn- interpret-loop [ast ctx]
  (let [data (:data ast)
        tuple (interpret-ast (first data) ctx)
        loop-type (-> data second :type)
        clauses (if (= loop-type :fn-clause)
                  [(-> data second :data)]
                  (into [] (map :data) (-> data second :data)))]
    (loop [input tuple]
      (let [output (loop [clause (first clauses)
                          clauses (rest clauses)]
                     (if clause
                       (let [pattern (first clause)
                             guard (if (= 3 (count clause))
                                     (second clause)
                                     nil)
                             body (peek clause)
                             new-ctx (volatile! {::parent ctx})
                             match? (match pattern input new-ctx)
                             success (:success match?)
                             clause-ctx (:ctx match?)]
                         (if success
                           (if guard 
                             (if (interpret-ast guard (volatile! (assoc clause-ctx ::parent ctx)))
                               (do
                                 (vswap! new-ctx #(merge % clause-ctx))
                                 (interpret-ast body new-ctx))
                               (recur (first clauses) (rest clauses)))
                             (do
                               (vswap! new-ctx #(merge % clause-ctx))
                               (interpret-ast body new-ctx)))
                           (recur (first clauses) (rest clauses))))

                       (throw (ex-info (str "Match Error: No match found in loop for " (show/show input)) {:ast ast}))))]
        (if (::data/recur output)
          (recur (:args output))
          output)))))

(defn- list-term [ctx]
  (fn [list member]
    (if (= (:type member) :splat)
      (let [splatted (interpret-ast (-> member :data first) ctx)
            splattable? (vector? splatted)
            tuple-splat? (= (first splatted) ::data/tuple)]
        (if splattable?
          (if tuple-splat?
            (into [::data/list] (concat list (rest splatted)))  
            (concat list splatted))
          (throw (ex-info "Cannot splat non-list into list" {:ast member}))))
      (conj list (interpret-ast member ctx)))))

(defn- interpret-list [ast ctx]
  (let [members (:data ast)]
    (into [::data/list] (reduce (list-term ctx) [] members))))

(defn- set-term [ctx]
  (fn [set member]
    (if (= (:type member) :splat)
      (let [splatted (interpret-ast (-> member :data first) ctx)
            splat-set? (set? splatted)]
        (if splat-set?
          (clojure.set/union set splatted)
          (throw (ex-info "Cannot splat non-set into set" {:ast member}))))
      (conj set (interpret-ast member ctx)))))

(defn- interpret-set [ast ctx]
  (let [members (:data ast)]
    (reduce (set-term ctx) #{} members)))

(defn- dict-term [ctx]
  (fn [dict member]
    (case (:type member)
      :splat (let [splatted (interpret-ast (-> member :data first) ctx)
                   splat-map? (or (::data/dict splatted)
                                (::data/struct splatted))]
               (if splat-map?
                 (merge dict splatted)
                 (throw (ex-info "Cannot splat non-dict into dict" {:ast member}))))
      :word (let [data (:data member) k (-> data first keyword)]
              (assoc dict k (interpret-ast member ctx)))

      :pair (let [data (:data member) k (-> data first :data first) v (second data)]
              (assoc dict k (interpret-ast v ctx))))))

(defn- interpret-dict [ast ctx]
  (let [members (:data ast)]
    (assoc (reduce (dict-term ctx) {} members) ::data/dict true)))

(defn- struct-term [ctx]
  (fn [struct member]
    (case (:type member)
      :splat (throw (ex-info "Cannot splat into struct" {:ast member}))

      :word (let [data (:data member) k (-> data first keyword)]
              (assoc struct k (interpret-ast member ctx)))

      :pair (let [data (:data member) k (-> data first :data first) v (second data)]
              (assoc struct k (interpret-ast v ctx))))))

(defn- interpret-struct [ast ctx]
  (let [members (:data ast)]
    (assoc (reduce (struct-term ctx) {} members) ::data/struct true)))

(defn- ns-term [ctx]
  (fn [ns member]
    (case (:type member)
      :splat (throw (ex-info "Cannot splat into ns" {:ast member}))

      :word (let [data (:data member) k (-> data first keyword)]
              (assoc ns k (interpret-ast member ctx)))

      :pair (let [data (:data member) k (-> data first :data first) v (second data)]
              (assoc ns k (interpret-ast v ctx))))))

(defn- interpret-ns [ast ctx]
  (let [data (:data ast)
        name (-> data first :data first)
        members (rest data)]
    (if (contains? @ctx name)
      (throw (ex-info (str "ns name " name " is already bound") {:ast ast}))
      (let [ns (merge {
                       ::data/struct true 
                       ::data/type ::data/ns
                       ::data/name name}
                 (reduce (ns-term ctx) {} members))]
        (vswap! ctx update-ctx {name ns})
        ns))))

(defn- interpret-literal [ast] (-> ast :data first))

(defn interpret-ast [ast ctx]
  (case (:type ast)

    (:nil :true :false :number :string :keyword) (interpret-literal ast)

    :let-expr (interpret-let ast ctx)

    :if-expr (interpret-if ast ctx)

    :word (resolve-word ast ctx)

    :synthetic (interpret-synthetic ast ctx)

    :match (interpret-match ast ctx)

    :when-expr (interpret-cond ast ctx)

    (:fn-named :lambda) (interpret-fn ast ctx)

    :do-expr (interpret-do ast ctx)

    :placeholder ::data/placeholder

    :ns-expr (interpret-ns ast ctx)

    :use-expr (interpret-use ast ctx)

    ;; :import-expr (interpret-import ast ctx)

    :ref-expr (interpret-ref ast ctx)

    ;:when-expr (interpret-ast (-> ast :data first) ctx)

    :recur-call
    {::data/recur true :args (interpret-ast (-> ast :data first) ctx)}

    :loop-expr (interpret-loop ast ctx)

    :block
    (let [exprs (:data ast)
          inner (pop exprs)
          last (peek exprs)
          ctx (volatile! {::parent ctx})]
      (run! #(interpret-ast % ctx) inner)
      (interpret-ast last ctx))

    :script
    (let [exprs (:data ast)
          inner (pop exprs)
          last (peek exprs)]
      (run! #(interpret-ast % ctx) inner)
      (interpret-ast last ctx))

    ;; note that, excepting tuples and structs,
    ;; runtime representations are bare
    ;; tuples are vectors with a special first member
    (:tuple :args)
    (let [members (:data ast)]
      (into [::data/tuple] (map #(interpret-ast % ctx)) members))

    :list-literal (interpret-list ast ctx)

    :set-literal (interpret-set ast ctx)

    :dict (interpret-dict ast ctx)

    :struct-literal
    (interpret-struct ast ctx)

    (throw (ex-info (str "Unknown AST node type " (get ast :type :err) " on line " (get-in ast [:token :line])) {:ast ast}))))

(defn get-line [source line]
  (if line
    (let [lines (clojure.string/split source #"\n")
          numlines (count lines)
          gettable? (> numlines line)]
      (if gettable? 
        (clojure.string/trim (nth lines (dec line)))
        nil))
    ))

(def runtime-error
  #?(
     :clj clojure.lang.ExceptionInfo
     :cljs js/Object
     ))

(defn- ns->ctx [ns]
  (into {} (map-keys kw->str) ns))

(def ludus-prelude 
  (let [scanned (scanner/scan prelude/prelude)
        parsed (p/apply-parser g/script (:tokens scanned))
        base-ctx (volatile! {::parent (volatile! {"base" base/base})})
        interpreted (interpret-ast parsed base-ctx)
        namespace (dissoc interpreted ::data/type ::data/name ::data/struct)
        context (ns->ctx namespace)]
    ; (println "Prelude fully loaded.")
    context))

; ;; TODO: update this to use new parser pipeline & new AST representation
; (defn interpret
;   ([source parsed] (interpret source parsed {}))
;   ([source parsed ctx]
;    (try
;      (let [base-ctx (volatile! {::parent (volatile! (merge ludus-prelude ctx))})]
;        (interpret-ast parsed base-ctx))
;      (catch #?(:cljs :default :clj Throwable) e
;        (println "Ludus panicked!")
;        (println "On line" (get-in (ex-data e) [:ast :token :line]))
;        (println ">>> " (get-line source (get-in (ex-data e) [:ast :token :line])))
;        (println (ex-message e))
;        (pp/pprint (ex-data e)
;          #?(:clj (System/exit 67))
;          )))))

; ;; TODO: update this to use new parser pipeline & new AST representation
; (defn interpret-file [source path parsed]
;   (try 
;     (let [base-ctx (volatile! {::parent (volatile! ludus-prelude) :file path})]
;       (interpret-ast parsed base-ctx))
;     (catch clojure.lang.ExceptionInfo e
;       (println "Ludus panicked in" path)
;       (println "On line" (get-in (ex-data e) [:ast :token :line]))
;       (println ">>> " (get-line source (get-in (ex-data e) [:ast :token :line])))
;       (println (ex-message e))
;       (System/exit 67))))

; ;; TODO: update this to use new parser pipeline & new AST representation
(defn interpret-repl
  ([parsed ctx]
   (let [orig-ctx @ctx]
     (try
       (let [result (interpret-ast parsed ctx)]
         {:result result :ctx ctx})
       (catch #?(:clj Throwable :cljs js/Object) e
         (println "Ludus panicked!")
         (println (ex-message e))
         {:result :error :ctx (volatile! orig-ctx)})))))

(defn interpret-safe [source parsed ctx]
  (let [base-ctx (volatile! {::parent (volatile! (merge ludus-prelude ctx))})]
    (try
      ;(println "Running source: " source)
      (interpret-ast parsed base-ctx)
      (catch #?(:clj Throwable :cljs js/Object) e
        (println "Ludus panicked!")
        (println "On line" (get-in (ex-data e) [:ast :token :line]))
        (println ">>> " (get-line source (get-in (ex-data e) [:ast :token :line])))
        (println (ex-message e))
        ;(pp/pprint (ex-data e))
        ;(throw e)
        {::data/error true 
         :line (get-in (ex-data e) [:ast :token :line])
         :message (ex-message e)}
        ))))

;; repl
(do
  
  (def source "1 2")

  (def tokens (-> source scanner/scan :tokens))

  (def ast (p/apply-parser g/script tokens))

  ;(def result (interpret-safe source ast {}))

  (-> ast prettify-ast println)

  ;(-> ast show/show-pattern println)
  
  )