(ns ludus.prelude
  (:require
    [ludus.data :as data]
    [ludus.show :as show]
    ;[ludus.draw :as d]
    ))

;; TODO: make eq, and, or special forms that short-circuit
;; Right now, they evaluate all their args
(def eq {:name "eq"
         ::data/type ::data/clj
         :body =})

(defn- id [x] x)

(def and- {:name "and"
           ::data/type ::data/clj
           :body (fn [& args] (every? id args))})

(def or- {:name "or"
          ::data/type ::data/clj
          :body (fn [& args] (some id args))})

(def add {:name "add"
          ::data/type ::data/clj
          :body +})

(def sub {:name "sub"
          ::data/type ::data/clj
          :body -})

(def mult {:name "mult"
           ::data/type ::data/clj
           :body *})

(def div {:name "div"
          ::data/type ::data/clj
          :body /})

(def gt {:name "gt"
         ::data/type ::data/clj
         :body >})

(def gte {:name "gte"
          ::data/type ::data/clj
          :body >=})

(def lt {:name "lt"
         ::data/type ::data/clj
         :body <})

(def lte {:name "lte"
          ::data/type ::data/clj
          :body <=})

(def inc- {:name "inc"
           ::data/type ::data/clj
           :body inc})

(def dec- {:name "dec"
           ::data/type ::data/clj
           :body dec})

(def ld-not {:name "not"
             ::data/type ::data/clj
             :body not})

(def panic! {:name "panic!"
             ::data/type ::data/clj
             :body (fn [& args] (throw (ex-info (apply show/show (interpose " " args)) {})))})

(defn- print-show [lvalue]
  (if (string? lvalue) lvalue (show/show lvalue)))

(def print- {:name "print"
             ::data/type ::data/clj
             :body (fn [& args]
                     (println (apply str (into [] (map print-show) args)))
                     :ok)})

(def deref- {:name "deref"
             ::data/type ::data/clj
             :body (fn [ref]
                     (if (::data/ref ref)
                       (deref (::data/value ref))
                       (throw (ex-info "Cannot deref something that is not a ref" {}))))})

(def set!- {:name "set!"
            ::data/type ::data/clj
            :body (fn [ref value]
                    (if (::data/ref ref)
                      (reset! (::data/value ref) value)
                      (throw (ex-info "Cannot set! something that is not a ref" {}))))})

(def show {:name "show"
           ::data/type ::data/clj
           :body ludus.show/show})

(def sleep- {:name "sleep"
             ::data/type ::data/clj
             :body (fn [ms] (Thread/sleep ms))})
(def conj- {:name "conj"
            ::data/type ::data/clj
            :body conj})

(def assoc- {:name "assoc"
             ::data/type ::data/clj
             :body assoc})

(def get- {:name "get"
           ::data/type ::data/clj
           :body (fn 
                   ([key, map]
                    (if (map? map)
                      (get map key)
                      nil))
                   ([key, map, default]
                    (if (map? map)
                      (get map key default)
                      default)))})

(def first- {:name "first"
             ::data/type ::data/clj
             :body (fn [v] (second v))})

(def rest- {:name "rest"
            ::data/type ::data/clj
            :body (fn [v]
                    (into [::data/list] (nthrest v 2)))})

(def nth- {:name "nth"
           ::data/type ::data/clj
           :body (fn 
                   ([i, xs]
                    (cond
                      (> 0 i) nil
                      (contains? xs (inc i)) (nth xs (inc i))
                      :else nil))
                   ([i, xs, default]
                    (cond
                      (> 0 i) default
                      (contains? xs (inc i)) (nth xs (inc i))
                      :else default)))})

(defn get-type [value]
  (let [t (type value)]
    (cond
      (nil? value) :nil

      (= clojure.lang.Keyword t) :keyword

      (= java.lang.Long t) :number

      (= java.lang.Double t) :number

      (= java.lang.String t) :string

      (= java.lang.Boolean t) :boolean

      (= clojure.lang.PersistentHashSet t) :set

      ;; tuples and lists
      (= clojure.lang.PersistentVector t)
      (if (= ::data/tuple (first value)) :tuple :list)

      ;; structs dicts namespaces refs
      (= clojure.lang.PersistentArrayMap t)
      (cond
        (::data/type value) (case (::data/type value)
                              (::data/fn ::data/clj) :fn
                              ::data/ns :ns)
        (::data/dict value) :dict
        (::data/struct value) :struct

        :else :none
        ))))

(def type- {:name "type"
            ::data/type ::data/clj
            :body get-type})

(defn strpart [kw] (->> kw str rest (apply str)))

(def clj {:name "clj"
          ::data/type ::data/clj
          :body (fn [& args]
                  (println "Args passed: " args)
                  (let [called (-> args first strpart read-string eval)
                        fn-args (rest args)]
                    (println "Fn: " called)
                    (println "Args: " fn-args)
                    (apply called fn-args)))})

(def count- {:name "count"
             ::data/type ::data/clj
             :body (fn [xs] (dec (count xs)))})

(def prelude {
              "id" id
              "eq" eq
              "add" add
              "print" print-
              "sub" sub
              "mult" mult
              "div" div
              "gt" gt
              "gte" gte
              "lt" lt
              "lte" lte
              "inc" inc-
              "dec" dec-
              "not" not
              "show" show
              "deref" deref-
              "set!" set!-
              "and" and-
              "or" or-
              "sleep" sleep-
              "assoc" assoc-
              "conj" conj-
              "get" get-
              "type" type-
              "clj" clj
              "first" first-
              "rest" rest-
              "nth" nth-
              "count" count-
              })