(ns ludus.show
  (:require
    [ludus.data :as data]
    ; [ludus.scanner :as s]
    ; [ludus.parser :as p]
    ; [ludus.grammar :as g]
    ; [ludus.interpreter :as i]
    [clojure.pprint :as pp]))

(declare show show-linear show-keyed)

(defn- show-vector [v]
  (if (= (first v) ::data/tuple)
    (str "(" (apply str (into [] show-linear (next v))) ")")
    (str "[" (apply str (into [] show-linear (next v))) "]")))

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
    (str "ref: " (::data/name v) " {" (-> v ::data/value deref show) "}")

    (::data/dict v)
    (str "#{" (apply str (into [] show-keyed (dissoc v ::data/dict))) "}")

    :else
    (with-out-str (pp/pprint v))))

(defn- show-set [v]
  (str "${" (apply str (into [] show-linear v)) "}"))

(defn show 
  ([v]
   (cond
     (string? v) (str "\"" v "\"")
     (number? v) (str v)
     (keyword? v) (str v)
     (boolean? v) (str v)
     (nil? v) "nil"
     (vector? v) (show-vector v)
     (set? v) (show-set v)
     (map? v) (show-map v)
     :else
     (with-out-str (pp/pprint v))
     ))
  ([v & vs] (apply str (into [] (comp (map show) (interpose " ")) (concat [v] vs))))
  )

(def show-linear (comp (map show) (interpose ", ")))

(def show-keyed (comp
                  (map #(str (show (first %)) " " (show (second %))))
                  (interpose ", ")))

(declare show-pattern)

(defn show-coll-pattern [pattern [start end]]
  (let [data (:data pattern)
        members (map show-pattern data)
        output (apply str (interpose ", " members))]
    (str start output end)))

(defn show-pattern [pattern]
  (case (:type pattern)
    nil ""

    :placeholder "_"

    :else "else"

    :true "true"

    :false "false"

    :nil "nil"

    :string (-> pattern :data first show)

    (:word :number :keyword) (-> pattern :data first str)

    :typed
    (let [word (-> pattern :data first :data first)
          type (-> pattern :data second :data first)]
      (str word " as " type))

    :splattern
    (let [splatted (-> pattern :data first show-pattern)]
      (str "..." splatted))

    :pair-pattern
    (let [key (-> pattern :data first)
          value (-> pattern :data second)]
      (str (show-pattern key) " " (show-pattern value)))

    :tuple-pattern (show-coll-pattern pattern ["(" ")"])

    :list-pattern (show-coll-pattern pattern ["[" "]"])

    :dict-pattern (show-coll-pattern pattern ["#{" "}"])

    :struct-pattern (show-coll-pattern pattern ["@{" "}"])

    ))

(comment
  (def source "let 1 = 0")
  (def tokens (-> source s/scan :tokens))
  (def ast (p/apply-parser g/script tokens))
  (println (i/prettify-ast ast))
  (println (-> ast :data first :data first show-pattern))
  )
