(ns ludus.prelude
  (:require
    [ludus.data :as data]
    [ludus.show :as show]
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
        :else :none
        ))))

(def type- {:name "type"
            ::data/type ::data/clj
            :body get-type})

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

(def clj {:name "clj"
          ::data/type ::data/clj
          :body (fn [& args]
                  (println "Args passed: " args)
                  (let [called (-> args first strpart readstr resolve-str)
                        fn-args (rest args)]
                    (println "Fn: " called)
                    (println "Args: " (clj->js fn-args))
                    #?(
                       :clj(apply called fn-args)
                       :cljs (.apply called js/window (clj->js fn-args)))))})

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