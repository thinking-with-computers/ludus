(ns ludus.prelude
  (:require
   [ludus.data :as data]))

(def eq {:name "eq"
         ::data/type ::data/clj
         :body =})

(def add {:name "add"
          ::data/type ::data/clj
          :body +})

(def panic {:name "panic"
            ::data/type ::data/clj
            :body (fn [& args] (throw (ex-info "Ludus panicked!" {:args args})))})

(def print {:name "print"
            ::data/type ::data/clj
            :body (fn [& args]
                    (println (str args))
                    :ok)})

(def prelude {"eq" eq
              "add" add
              "panic" panic
              "print" print})