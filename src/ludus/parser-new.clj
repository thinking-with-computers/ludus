(ns ludus.parser-new
	(:require
		[ludus.scanner :as scan]))

(defn ok? [[ok]]
	(= ok :ok))

(defn kw->type [kw] (apply str (next (str kw))))

(defn match [kw token]
	(if (= kw (:type token))
		[:ok token]
		[:error token (str "Expected " (kw->type kw))]))

(defn parser 
	([kw] {:type kw :fn #(match kw %)})
	([kw err] {:type kw :fn #(assoc (match kw %) 2 err)}))


(defn choice [& args])

(def eg (:tokens (scan/scan "123 :foo")))

(def word (parser :word "fuck"))

(word (first eg))

(comment

(def string (parser :string))

)