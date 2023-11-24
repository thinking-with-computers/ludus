(ns ludus.web
  (:require 
   	[ludus.core :as core]
   	[goog.object :as o]
   	)
  )

(defn get-element [id]
 	(.getElementById (.-document js/window) id))

(def canv (get-element "canv"))

(def code (get-element "code"))

(def out (get-element "output"))

(def play (get-element "play"))

(defn run-code []
 	(let [source (.-value code)
      		result (core/run source)]
  		(println "Running code:" source)
  		(o/set out "value" result)))

(.addEventListener play "click" run-code)

(defn setup []
 	(js/createCanvas 640 240 canv)
 	(js/background 235)
 	)

(defn draw []
 	(if js/mouseIsPressed
  		(js/fill 255)
  		(js/fill 155))
 	(js/ellipse js/mouseX js/mouseY 80 80))

(doto js/window
 	(o/set "setup" setup)
 	(o/set "draw" draw))