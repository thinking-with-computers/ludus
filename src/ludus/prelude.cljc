(ns ludus.prelude
  (:require
   [ludus.data :as data]
   [ludus.show]))

(def eq {:name "eq"
         ::data/type ::data/clj
         :body =})

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
             :body (fn [& args] (throw (ex-info (apply str (interpose " " args)) {})))})

(def print- {:name "print"
            ::data/type ::data/clj
            :body (fn [& args]
                    (println (apply str args))
                    :ok)})

(declare show)

(defn- show-vector [v]
  (if (= (first v) ::data/tuple)
    (str "(" (apply str (into [] (comp (map (:body show)) (interpose ", ")) (next v))) ")")
    (str "[" (apply str (into [] (comp (map (:body show)) (interpose ", ")) v)) "]")))

(def show {:name "show"
  ::data/type ::data/clj
  :body ludus.show/show})

(def prelude {"eq" eq
              "add" add
              "panic!" panic!
              "print" print-
              "sub" sub
              "mult" mult
              "div" div
              "inc" inc-
              "dec" dec-
              "not" not
              "show" show})