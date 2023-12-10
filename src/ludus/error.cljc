(ns ludus.error
 	(:require [clojure.string :as string]))

(defn get-line [source {:keys [line]}]
 	(let [lines (string/split-lines source)
     			the_line (nth lines (dec line))]
 			the_line))

(defn get-underline [source {:keys [line start lexeme]} prefix]
 	(let [lines (string/split-lines source)
      		lines-before (subvec lines 0 (dec line))
      		line-start (reduce (fn [len line] (+ len (count line))) (count lines-before) lines-before)
      		from-start (- start line-start)
      		underline-length (count lexeme)
      		padding (string/join (take (+ prefix from-start) (repeat "-")))
      		underline (string/join (take underline-length (repeat "^")))]
  		(apply str padding underline)
  		))

(defn scan-error [] :TODO)

(defn parse-error [source {:keys [trace token]}]
 	(let [line (get-line source token)
      		line-num (:line token)
      		prefix (str line-num ": ")
      		underline (get-underline source token (count prefix))
      		expected (first trace)
      		got (:type token)
      		message (str "Ludus found a parsing error on line " line-num ".\nExpected: " expected "\nGot: " got "\n")
      		]
  		(str message "\n" prefix line "\n" underline)
  		)
 	)

(defn run-error [source {:keys [line message]}]
 	(if line
  		(str "Ludus panicked on line " line ":\n" (get-line source {:line line}) "\n" message)
  		(str "Ludus panicked!\n" message)
  		))
