(ns ludus.prelude
  (:require
    [ludus.data :as data]
    [ludus.show :as show]
    [ludus.draw :as d]))

;; TODO: make eq, and, or special forms that short-circuit
;; Right now, they evaluate all their args
(def eq {:name "eq"
         ::data/type ::data/clj
         :body =})

(defn- id [x] x)

(def and- {:name "and"
           ::data/type ::data/clj
           :body (fn [&args] (every? id &args))})

(def or- {:name "or"
          ::data/type ::data/clj
          :body (fn [&args] (some id &args))})

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
           :body get})

(def draw {:name "draw"
  ::data/type ::data/clj
  :body d/ludus-draw})

(def prelude {"eq" eq
              "add" add
              "print" print-
              "sub" sub
              "mult" mult
              "div" div
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
              "draw" draw
              })