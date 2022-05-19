(ns ludus.show
  (:require
    [ludus.data :as data]
    [clojure.pprint :as pp]))

(declare show show-linear show-keyed)

(defn- show-vector [v]
  (if (= (first v) ::data/tuple)
    (str "(" (apply str (into [] show-linear (next v))) ")")
    (str "[" (apply str (into [] show-linear v)) "]")))

(defn- show-map [v]
  (cond
    (or (= (::data/type v) ::data/fn)
      (= (::data/type v) ::data/clj))
    (str "fn " (:name v))

    (= (::data/type v) ::data/ns)
    (str "ns " (::data/name v) " {"
      (apply str (into [] show-keyed (dissoc v ::data/struct ::data/type ::data/name)))
      "}")

    (::data/struct v) 
    (str "@{" (apply str (into [] show-keyed (dissoc v ::data/struct))) "}")

    (::data/ref v) ;; TODO: reconsider this
    (str "ref:" (::data/name v) " <" (deref (::data/value v))">")

    (::data/hashmap v)
    (str "#{" (apply str (into [] show-keyed (dissoc v ::data/hashmap))) "}")

    :else
    (pp/pprint v)

    ))

(defn- show-set [v]
  (str "${" (apply str (into [] show-linear v)) "}"))

(defn show [v]
  (cond
    (string? v) (str "\"" v "\"")
    (number? v) (str v)
    (keyword? v) (str v)
    (boolean? v) (str v)
    (nil? v) "nil"
    (vector? v) (show-vector v)
    (set? v) (show-set v)
    (map? v) (show-map v)
    :else (with-out-str (pp/pprint v))))

(def show-linear (comp (map show) (interpose ", ")))

(def show-keyed (comp 
                  (map #(str (show (first %)) " " (show (second %))))
                  (interpose ", ")))

(show {::data/type ::data/fn :name "foo"})
