(ns ludus.turtle-graphics
 	(:require [quil.core :as q]
   	[quil.middleware :as m]))

(defn setup []
 	(q/frame-rate 60)
 	(q/color-mode :rgb)
 	{:position [0 0]
 		:path []})

(def path (atom []))

(defn turns->degrees [turns] (mod (+ 90 (* -360 turns)) 360))

(turns->degrees -0.25)

(defn degrees->turns [degs] (mod (/ (- degs 90) -360) 1))

(degrees->turns 360)

(defn add [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])

(defn sub [[x1 y1 [x2 y2]]] [(- x1 x2) (- y1 y2)])

(defn scale [[x y] m] [(* x m) (* y m)])

(defn heading->vec [heading]
 	(let [degs (turns->degrees heading)]
  		[(Math/cos degs) (Math/sin degs)]))

(defn vec->heading [[x y]]
 	(degrees->turns (Math/atan2 x y)))

(defn forward [steps]
 	(let [prev (peek @path)
      		heading (:heading prev)
      		unit (heading->vec heading)
      		move (scale unit steps)
     	 	curr (update prev :postion #(add % move))]
  		(swap! path #(conj % curr))))

(defn back [steps] (forward (* -1 steps)))

(defn right [turns] 
 	(let [prev (peek @path)
      		curr (update prev :heading #(- % turns))]
  		(swap! path #(conj % curr))))

(defn left [turns] (right (* -1 turns)))

(comment
 	:path {:start [x y] :end [x y] :color [R G B]}
 	)

(defn update-state [state]
 	{:color (mod (+ (:color state) 0.7) 255)
 	 :angle (+ (:angle state) 0.1)})

(defn draw-state [state]
 	(q/background 240)
 	(q/fill (:color state) 255 255)
 	(let [angle (:angle state)
      		x (* 150 (q/cos angle))
      		y (* 150 (q/sin angle))]
 	  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
    		(q/ellipse x y 100 100))))

(defn ludus-draw []
 	(q/defsketch sketch
   	:title "Hello Ludus"
   	:size [500 500]
   	:setup setup
   	:update update-state
   	:draw draw-state
   	:features []
   	:middleware [m/fun-mode])
 	:ok)