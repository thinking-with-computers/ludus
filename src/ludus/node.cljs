(ns ludus.node
 	; (:require [ludus.interpreter :as i]
 	; [ludus.grammar :as g]
 	; [ludus.parser :as p]
 	; [ludus.scanner :as s])
 	)

(defn run [source]
 	(println "Running source!")
 	(println "(This is a stub.)")
 	(clj->js {
          		:result "Hello, world!"
          		:errors [{:msg "An error"}, {:msg "Another error"}]
          		:console ["Here's a log.", "Here's another log."]
          		:draw [[:background 0]
                			[:fill 255]
                			[:rect 10 10 50 50]]}))