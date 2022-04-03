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
(defn- resolve [word ctx-atom]
  (let [ctx @ctx-atom]
    (if (contains? ctx word)
      (get ctx word)
      (if (contains? ctx ::parent)
        (recur word (::parent ctx))
        (throw (new Exception (str "Unbound name: " word)))))))

(declare interpret match)

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
        value (interpret expr ctx)
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
        if-value (interpret if-expr ctx)]
    (if if-value
      (interpret then-expr ctx)
      (interpret else-expr ctx))))

(defn- interpret-match [ast ctx]
  (let [match-expr (:expr ast)
        expr (interpret match-expr ctx)
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
              (interpret body new-ctx))
            (recur (first clauses) (rest clauses))))
        (throw (ex-info "Match Error: No match found" {}))))))

(defn- interpret-called-kw [kw tuple ctx]
  ;; TODO: check this statically
  (if (not (= 1 (:length tuple)))
    (throw (ex-info "Called keywords must be unary" {}))
    (let [kw (interpret kw ctx)
          map (second (interpret tuple ctx))]
      (if (::ast/struct map)
        (if (contains? map kw)
          (kw map)
          (throw (ex-info (str "Struct error: no member at " kw) {})))
        (get map kw))
      )))

(defn- call-fn [fn tuple ctx]
  (let [passed (interpret tuple ctx)]
    (case (::data/type fn)
      ::data/clj (apply (:body fn) (next passed))

      ::data/fn
      (let [clauses (:clauses fn)]
        (loop [clause (first clauses)
               clauses (rest clauses)]
          (if clause
            (let [pattern (:pattern clause)
                  body (:body clause)
                  new-ctx (atom {::parent ctx})
                  match? (match pattern passed new-ctx)
                  success (:success match?)
                  clause-ctx (:ctx match?)]
              (if success
                (do
                  (swap! new-ctx #(merge % clause-ctx))
                  (interpret body new-ctx))
                (recur (first clauses) (rest clauses))))

            (throw (ex-info "Match Error: No match found" {:fn-name (:name fn)})))))

      (throw (ex-info "I don't know how to call that" {:fn fn})))))

;; TODO: add placeholder partial application
(defn- interpret-synthetic-term [prev-value curr ctx]
  (let [type (::ast/type curr)]
    (if (= type ::ast/atom)
      (if (::ast/struct prev-value)
        (if (contains? prev-value (:value curr))
          (get prev-value (:value curr))
          (throw (ex-info (str "Struct error: no member " (:value curr)) {}))))
      (call-fn prev-value curr ctx))))

(defn- interpret-synthetic [ast ctx]
  (let [terms (:terms ast)
        first (first terms)
        second (second terms)
        rest (rest (rest terms))
        first-term-type (::ast/type first)
        first-val (if (= first-term-type ::ast/atom)
                    (interpret-called-kw first second ctx)
                    (interpret-synthetic-term (interpret first ctx) second ctx))]
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

(defn- map-values [f]
  (map (fn [kv]
         (let [[k v] kv]
           [k (f v)]))))

(defn interpret [ast ctx]
  (case (::ast/type ast)

    ::ast/atom (:value ast)

    ::ast/word (resolve (:word ast) ctx)

    ::ast/let (interpret-let ast ctx)

    ::ast/if (interpret-if ast ctx)

    ::ast/match (interpret-match ast ctx)

    ::ast/synthetic (interpret-synthetic ast ctx)

    ::ast/fn (interpret-fn ast ctx)

    ::ast/block
    (let [exprs (:exprs ast)
          inner (pop exprs)
          last (peek exprs)
          ctx (atom {::parent ctx})]
      (run! #(interpret % ctx) inner)
      (interpret last ctx))

    ::ast/script
    (let [exprs (:exprs ast)
          inner (pop exprs)
          last (peek exprs)
          ctx (atom prelude/prelude)]
      (run! #(interpret % ctx) inner)
      (interpret last ctx))

    ;; note that, excepting a tuple,
    ;; runtime representations are bare
    ;; tuples are vectors with a special first member
    ::ast/tuple
    (let [members (:members ast)]
      (into [::data/tuple] (map #(interpret % ctx)) members))

    ::ast/list
    (let [members (:members ast)]
      (into [] (map #(interpret % ctx)) members))

    ::ast/set
    (let [members (:members ast)]
      (into #{} (map #(interpret % ctx)) members))

    ::ast/hash
    (let [members (:members ast)]
      (into {} (map-values #(interpret % ctx)) members))

    ::ast/struct
    (let [members (:members ast)]
      (into {::ast/struct true} (map-values #(interpret % ctx)) members))

    (throw (ex-info "Unknown AST node type" {:node ast}))))

(do

  (def source "

    let s = @{:foo @{:baz 12}, :bar 23}
    s :foo :baz

	")

  (println "")
  (println "****************************************")
  (println "*** *** NEW INTERPRETATION *** ***")
  (println "")

  (-> source
      (scanner/scan)
      (parser/parse)
      (::parser/ast)
      (interpret {})
      (pp/pprint)))

(comment "

	Left to do:
	* if-let pattern
	* improve panics
	* add location info for panics

")

(defn fib [n]
  (case n
    0 1

    1 1

    (+ (fib (- n 1)) (fib (- n 2)))))

(fib 35)





