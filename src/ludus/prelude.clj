(ns ludus.prelude
  (:require
   [ludus.data :as data]))

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

(def prelude {"eq" eq
              "add" add
              "panic!" panic!
              "print" print-
              "sub" sub
              "mult" mult
              "div" div
              "inc" inc-
              "dec" dec-
              "not" not})