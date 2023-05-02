(ns ludus.draw
 	(:require [quil.core :as q]
   	[quil.middleware :as m]))

(defn setup []
 	(q/frame-rate 60)
 	(q/color-mode :hsb)
 	{:color 0 :angle 0})

(defn update-state [state]
 	{:color (mod (+ (:color state) 0.7) 255)
 	 :angle (+ (:angle state) 0.1)})

(defn draw-state [state]
 	(q/background 240)
 	(q/fill (:color state) 255 255)
 	(let [angle (:angle state)
      		x (* 150 (q/cos angle))
      		y (* 150 (q/sin angle))]
 	  (q/with-translation [(/ (q/width) 2)
                	  						(/ (q/height) 2)]
    		(q/ellipse x y 100 100))))

(defn ludus-draw []
 	(q/defsketch sketch
   	:title "Hello Ludus"
   	:size [500 500]
   	:setup setup
   	:update update-state
   	:draw draw-state
   	:features []
   	:middleware [m/fun-mode]))
