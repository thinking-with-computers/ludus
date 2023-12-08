(ns ludus.base
  (:require
    [ludus.data :as data]
    [ludus.show :as show]
    [clojure.math :as math]
    ;[ludus.draw :as d]
    #?(:cljs [cljs.reader])
    #?(:cljs [goog.object :as o])
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

(def not- {:name "not"
           ::data/type ::data/clj
           :body not})

(defn- print-show [lvalue]
  (if (string? lvalue) lvalue (show/show lvalue)))

(defn- stringify-args [arglist]
  (apply str (interpose " " (into [] (map print-show) (rest arglist)))))

(def panic! {:name "panic!"
             ::data/type ::data/clj
             :body (fn panic-inner
                     ([] (panic-inner [::data/list])) 
                     ([args] (throw (ex-info (stringify-args args) {}))))})

(def print- {:name "print"
             ::data/type ::data/clj
             :body (fn [args]
                     (println (stringify-args args))
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

(def conj- {:name "conj"
            ::data/type ::data/clj
            :body conj})

(def assoc- {:name "assoc"
             ::data/type ::data/clj
             :body assoc})

(def dissoc- {name "dissoc"
              ::data/type ::data/clj
              :body dissoc})

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

(def rest- {:name "rest"
            ::data/type ::data/clj
            :body (fn [v]
                    (into [::data/list] (nthrest v 2)))})

(def nth- {:name "nth"
           ::data/type ::data/clj
           :body nth})

(def slice {:name "slice"
            ::data/type ::data/clj
            :body subvec})

(def types {
            :keyword
            #?(
               :clj clojure.lang.Keyword
               :cljs cljs.core/Keyword
               )

            :long
            #?(
               :clj java.lang.Long
               :cljs js/Number
               )

            :double
            #?(
               :clj java.lang.Double
               :cljs js/Number
               )

            :integer
            #?(
               :clj java.lang.Integer
               :cljs js/Number
               )

            :ratio
            #?(
               :clj clojure.lang.Ratio
               :cljs js/Number
               )

            :string
            #?(
               :clj java.lang.String
               :cljs js/String
               )

            :boolean
            #?(
               :clj java.lang.Boolean
               :cljs js/Boolean
               )

            :set
            #?(
               :clj clojure.lang.PersistentHashSet
               :cljs cljs.core/PersistentHashSet
               )

            :vector
            #?(
               :clj clojure.lang.PersistentVector
               :cljs cljs.core/PersistentVector
               )

            :map
            #?(
               :clj clojure.lang.PersistentArrayMap
               :cljs cljs.core/PersistentArrayMap
               )
            })

(defn get-type [value]
  (let [t (type value)]
    (cond
      (nil? value) :nil

      (= (:keyword types) t) :keyword

      (= (:long types) t) :number

      (= (:double types) t) :number

      (= (:integer types) t) :number

      (= (:ratio types) t) :number

      (= (:string types) t) :string

      (= (:boolean types) t) :boolean

      (= (:set types) t) :set

      ;; tuples and lists
      (= (:vector types) t)
      (if (= ::data/tuple (first value)) :tuple :list)

      ;; structs dicts namespaces refs
      (= (:map types) t)
      (cond
        (::data/type value) (case (::data/type value)
                              (::data/fn ::data/clj) :fn
                              ::data/ns :ns)
        (::data/dict value) :dict
        (::data/struct value) :struct
        (::data/ref value) :ref
        :else :none
        ))))

(def type- {:name "type"
            ::data/type ::data/clj
            :body get-type})

(defn- kv->tuple [[k v]] [::data/tuple k v])

(def to_list {name "to_list"
              ::data/type ::data/clj
              :body (fn [item]
                      (case (get-type item)
                        (:number :nil :boolean :fn :string :ref :keyword) [::data/list item]
                        :list item
                        :set (into [::data/list] item)
                        :tuple (into [::data/list] (rest item))
                        :dict (into [::data/list] (map kv->tuple) (dissoc item ::data/dict))
                        :struct (into [::data/list] (map kv->tuple) (dissoc item ::data/struct))
                        :ns (into [::data/list] (map kv->tuple) (dissoc item ::data/struct ::data/type ::data/name))
                        ))})

(def to_dict {name "to_dict"
              ::data/type ::data/clj
              :body (fn [struct] (-> struct (assoc ::data/dict true) (dissoc ::data/struct ::data/type ::data/name)))})

(defn strpart [kw] (->> kw str rest (apply str)))

(def readstr
  #?(
     :clj read-string
     :cljs cljs.reader/read-string
     ))

(defn- resolve-str [str]
  #?(
     :clj (eval str)
     :cljs (.bind (o/get js/window str) js/window)
     ))

(def extern {:name "extern"
             ::data/type ::data/clj
             :body (fn [& args]
                     ;(println "Args passed: " args)
                     (let [called (-> args first strpart readstr resolve-str)
                           fn-args (rest args)]
                       ;(println "Fn: " called)
                       ;(println "Args: " (clj->js fn-args))
                       #?(
                          :clj (apply called fn-args)
                          :cljs (.apply called js/window (clj->js fn-args)))))})

(def count- {:name "count"
             ::data/type ::data/clj
             :body count})

(def into- {:name "into"
            ::data/type ::data/clj
            :body into})

(def to_vec {:name "to_vec"
             ::data/type ::data/clj
             :body (fn [xs] (into [] (dissoc xs ::data/type ::data/struct ::data/name)))})

(def fold {:name "fold"
           ::data/type ::data/clj
           :body reduce})

(def map- {:name "map"
           ::data/type ::data/clj
           :body map})

(def prn- {:name "raw"
           ::data/type ::data/clj
           :body println})

(def concat- {:name "concat"
              ::data/type ::data/clj
              :body (fn [xs ys]
                      (if (= ::data/list (first xs))
                        (into [::data/list] (concat (rest xs) (rest ys)))
                        (into #{} (concat xs ys))))})

(def str- {:name "str"
           ::data/type ::data/clj
           :body str})

(def doc- {:name "doc"
           ::data/type ::data/clj
           :body (fn [f]
                   (let [name (:name f)
                         docstring (:doc f)
                         clauses (:clauses f)
                         patterns (map first clauses)
                         pretty-patterns (map show/show-pattern patterns)
                         doc (into [name docstring] pretty-patterns)]
                     (apply str (interpose "\n" doc)))
                   )})

(def sin {:name "sin"
          ::data/type ::data/clj
          :body math/sin})

(def cos {:name "cos"
          ::data/type ::data/clj
          :body math/cos})

(def tan {:name "tan"
          ::data/type ::data/clj
          :body math/tan})

(def atan_2 {:name "atan_2"
             ::data/type ::data/clj
             :body math/atan2})

(def sqrt {:name "sqrt"
           ::data/type ::data/clj
           :body math/sqrt})

(def random {:name "random"
             ::data/type ::data/clj
             :body rand})

(def floor {:name "floor"
            ::data/type ::data/clj
            :body math/floor})

(def ceil {:name "ceil"
           ::data/type ::data/clj
           :body math/ceil})

(def round {:name "round"
            ::data/type ::data/clj
            :body math/round})

(def range- {:name "range"
             ::data/type ::data/clj
             :body (fn [start end] (into [::data/list] (range (-> start math/ceil int) end)))})

(def base {
           :id id
           :eq eq
           :add add
           :print print-
           :sub sub
           :mult mult
           :div div
           :gt gt
           :gte gte
           :lt lt
           :lte lte
           :inc inc-
           :dec dec-
           :not not-
           :show show
           :deref deref-
           :set! set!-
           :and and-
           :or or-
           :assoc assoc-
           :dissoc dissoc-
           :conj conj-
           :get get-
           :type type-
           :extern extern
           :rest rest-
           :nth nth-
           :slice slice
           :count count-
           :into into-
           :to_vec to_vec
           :fold fold
           :map map
           :panic! panic!
           :prn prn-
           :concat concat-
           :str str-
           :to_list to_list
           :doc doc-
           :pi math/PI
           :sin sin
           :cos cos
           :tan tan
           :atan_2 atan_2
           :sqrt sqrt
           :random random
           :ceil ceil
           :floor floor
           :round round
           :range range-
           })