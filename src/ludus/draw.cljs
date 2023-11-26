(ns ludus.draw)

(defn background
 	([x] (js/background x))
 	([r g b] (js/background r g b))
 	([r g b a] (js/background r g b a)))

(defn rect [[x y] [w h]]
 	(js/rect x y w h))

(defn ellipse [[x y] [w h]])

(def draw {

          	})