(ns ludus.repl
	(:require 
		[ludus.scanner :as scanner]
		[ludus.parser :as parser]
		[ludus.interpreter :as interpreter]
		[ludus.show :as show]))

(defn launch []
	(println "Welcome to Ludus (v. 0.1.0-alpha)")
	(println "ludus=>")
	(System/exit 0))