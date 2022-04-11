(ns ludus.interpreter
  (:require
    [ludus.parser :as parser]
    [ludus.scanner :as scanner]
    [ludus.ast :as ast]
    [ludus.prelude :as prelude]
    [ludus.data :as data]
    [clojure.pprint :as pp]))

;; right now this is not very efficient:
;; it's got runtime checking
;; we should be able to do these checks statically
;; that's for later, tho
(defn- resolve-word[word ctx-atom]
  (let [ctx @ctx-atom]
    (if (contains? ctx word)
      (get ctx word)
      (if (contains? ctx ::parent)
        (recur word (::parent ctx))
        (throw (ex-info (str "Unbound name: " word) {}))))))

(declare interpret-ast match interpret)

(defn- match-tuple [pattern value ctx-atom]
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
        (let [match? (match (nth members (dec i)) (nth value i) ctx-atom)]
          (if (:success match?)
            (recur (dec i) (merge ctx (:ctx match?)))
            {:success false :reason (str "Could not match " pattern " with " value)})))))))

(defn- match [pattern value ctx-atom]
  (let [ctx @ctx-atom]
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

      ::ast/tuple (match-tuple pattern value ctx-atom)

      (throw (ex-info "Unknown pattern" {:pattern pattern})))))

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
      (swap! ctx update-ctx (:ctx match))
      (throw (ex-info (:reason match) {})))
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
        new-ctx (atom {::parent ctx})
        match? (match pattern expr new-ctx)
        success (:success match?)
        clause-ctx (:ctx match?)]
        (if success
          (do
            (swap! new-ctx #(merge % clause-ctx))
            (interpret-ast body new-ctx))
          (recur (first clauses) (rest clauses))))
      (throw (ex-info "Match Error: No match found" {}))))))

(defn- interpret-cond [ast ctx]
  (let [clauses (:clauses ast)]
    (loop [clause (first clauses)
      clauses (rest clauses)]
      (if (not clause)
        (throw (ex-info "Cond Error: No match found" {}))
        (let [test-expr (:test clause)
          body (:body clause)
          truthy? (boolean (interpret-ast test-expr ctx))]
          (if truthy?
            (interpret-ast body ctx)
            (recur (first clauses) (rest clauses))
            )
          )
        )
      )))

(defn- interpret-called-kw [kw tuple ctx]
  ;; TODO: check this statically
  (if (not (= 1 (:length tuple)))
    (throw (ex-info "Called keywords must be unary" {}))
    (let [kw (interpret-ast kw ctx)
      map (second (interpret-ast tuple ctx))]
      (if (::data/struct map)
        (if (contains? map kw)
          (kw map)
          (throw (ex-info (str "Struct error: no member at " kw) {})))
        (get map kw))
      )))

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
     (let [clauses (:clauses lfn)]
      (loop [clause (first clauses)
       clauses (rest clauses)]
       (if clause
        (let [pattern (:pattern clause)
          body (:body clause)
          new-ctx (atom {::parent ctx})
          match? (match pattern tuple new-ctx)
          success (:success match?)
          clause-ctx (:ctx match?)]
          (if success
            (do
              (swap! new-ctx #(merge % clause-ctx))
              (interpret-ast body new-ctx))
            (recur (first clauses) (rest clauses))))

        (throw (ex-info "Match Error: No match found" {:fn-name (:name lfn)})))))

     (keyword? lfn)
     (if (= 2 (count tuple))
      (let [target (second tuple) kw lfn]
        (if (::data/struct target)
          (if (contains? target kw)
            (kw target)
            (throw (ex-info (str "Struct error: no member at " kw) {})))
          (kw target)))
      (throw (ex-info "Called keywords take a single argument" {})))

     :else (throw (ex-info "I don't know how to call that" {:fn lfn}))))

;; TODO: add placeholder partial application
(defn- interpret-synthetic-term [prev-value curr ctx]
  (let [type (::ast/type curr)]
    (if (= type ::ast/atom)
      (if (::data/struct prev-value)
        (if (contains? prev-value (:value curr))
          (get prev-value (:value curr))
          (throw (ex-info (str "Struct error: no member " (:value curr)) {})))
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

(defn- interpret-fn [ast ctx]
  (let [name (:name ast)
    clauses (:clauses ast)]
    (if (= name ::ast/anon)
      {::data/type ::data/fn
       :name name
       :clauses clauses}
       (let [fn {::data/type ::data/fn
        :name name
        :clauses clauses}]
        (if (contains? @ctx name)
          (throw (ex-info (str "Name " name " is already bound") {}))
          (do
            (swap! ctx update-ctx {name fn})
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
      (throw (ex-info (str "ns name " name " is already bound") {}))
      (let [ns (into
        {::data/struct true ::data/type ::data/ns ::data/name name}
        (map-values #(interpret-ast % ctx))
        members)]
      (do
        (swap! ctx update-ctx {name ns})
        ns)))))

(defn- interpret-import [ast ctx]
  (let [path (:path ast)
    name (:name ast)]
    (if (contains? @ctx name)
      (throw (ex-info (str "Name " name " is alrady bound") {}))
  (let [result  ;; TODO: add any error handling at all   
    (-> path
          (slurp)
          (scanner/scan)
          (parser/parse)
          (interpret))]
    (swap! ctx update-ctx {name result})
    result ;; TODO: test this!
    ))))

(defn interpret-ast [ast ctx]
  (case (::ast/type ast)

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

    ::ast/block
    (let [exprs (:exprs ast)
      inner (pop exprs)
      last (peek exprs)
      ctx (atom {::parent ctx})]
      (run! #(interpret-ast % ctx) inner)
      (interpret-ast last ctx))

    ::ast/script
    (let [exprs (:exprs ast)
      inner (pop exprs)
      last (peek exprs)
      ctx (atom prelude/prelude)]
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

    ::ast/list
    (let [members (:members ast)]
      (into [] (map #(interpret-ast % ctx)) members))

    ::ast/set
    (let [members (:members ast)]
      (into #{} (map #(interpret-ast % ctx)) members))

    ::ast/hash
    (let [members (:members ast)]
      (into {} (map-values #(interpret-ast % ctx)) members))

    ::ast/struct
    (let [members (:members ast)]
      (into {::data/struct true} (map-values #(interpret-ast % ctx)) members))

    (throw (ex-info "Unknown AST node type" {:node ast}))))

(defn interpret [parsed]
  (interpret-ast (::parser/ast parsed) {}))

(comment

  (def source "

    let foo = 2

    match foo with {
      1 -> :one
      2 -> :two
      else -> :oops
    }

    ns bar {
      foo
    }

    bar :foo

    ")

  (println "")
  (println "****************************************")
  (println "*** *** NEW INTERPRETATION *** ***")
  (println "")

  (-> source
    (scanner/scan)
    (parser/parse)
    (interpret)
    (pp/pprint)))

(comment "

	Left to do:
	* if-let pattern
	* improve panics
	* add location info for panics
  * refactor calling keywords
  * refactor accessing structs vs. hashes

  ")





