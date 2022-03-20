(ns ludus.prelude
	(:require
			[ludus.ast :as ast]))

(def eq {
	:name "eq"
	::ast/type ::ast/clj
	:body =
	})

(def add {
	:name "add"
	::ast/type ::ast/clj
	:body +
	})

(def panic {
	:name "panic"
	::ast/type ::ast/clj
	:body (fn [& args] (throw (ex-info "Ludus panicked!" {:args args})))
	})

(def print {
	:name "print"
	::ast/type ::ast/clj
	:body (fn [& args] 
		(println (str args))
		:ok)
	})

(def prelude {
	"eq" eq 
	"add" add 
	"panic" panic
	"print" print
	})