(ns ludus.scanner
  (:require [ludus.token :as token]
            [clojure.pprint :as pp]))

(def reserved-words
  "List of Ludus reserved words."
  #{"fn"
    "if"
    "then"
    "else"
    "nil"
    "match"
    "true"
    "false"
    "loop"
    "recur"
    "as"
    "ref"
    "mut"})
    ;; other possibilities
    ;; "pattern" -- first class patterns?
    

(defn- new-scanner 
  "Creates a new scanner."
  [source]
  {::source source
   ::length (count source)
   ::errors []
   ::start 0
   ::current 0
   ::line 1
   ::tokens []})

(defn- at-end? 
  "Tests if a scanner is at end of input."
  [scanner]
  (>= (::current scanner) (::length scanner)))

(defn- current-char 
  "Gets the current character of the scanner."
  [scanner]
  (nth (::source scanner) (::current scanner) nil))

(defn- advance 
  "Advances the scanner by a single character."
  [scanner]
  (update scanner ::current inc))

(defn- next-char 
  "Gets the next character from the scanner."
  [scanner]
  (current-char (advance scanner)))

(defn- current-lexeme 
  [scanner]
  (subs (::source scanner) (::start scanner) (::current scanner)))

(defn- char-in-range? [start end char]
  (and char
       (>= (int char) (int start))
       (<= (int char) (int end))))

(defn- digit? [c]
  (char-in-range? \0 \9 c))

(defn- nonzero-digit? [c]
  (char-in-range? \1 \9 c))

(defn- alpha? [c]
  (boolean (re-find #"\p{L}" (str c))))

(defn- whitespace? [c]
  (or (= c \space) (= c \tab)))

(def terminators #{\: \; \newline \{ \( \[ \$ \# \- \< \& \,})

(defn- terminates? [c]
  (or (whitespace? c) (contains? terminators c)))

(defn- add-token
  ([scanner token-type]
   (add-token scanner token-type nil))
  ([scanner token-type literal]
   (update scanner ::tokens conj (token/token token-type (current-lexeme scanner) literal (::line scanner)))))

(defn- add-error [scanner msg]
  (update scanner ::errors conj {:msg msg :line (::line scanner)}))

(defn- scan-keyword 
  ([scanner] (scan-keyword scanner scanner))
  ([start current]))

(defn- add-keyword [scanner]
  (let [advanced (advance scanner)
        char (current-char advanced)]
    (if (not (alpha? char))
      (add-error scanner (str "Keywords must start with a letter, e.g. :foo. Got " \: char))
      (scan-keyword advanced))))

(defn- add-zero-start [scanner])

(defn- add-number [scanner]
  (let [current (current-char scanner)]
    (if (nonzero-digit? current)
      (loop [current current]))))

;; I am working here--trying to figure out how to add a string token
(defn- add-string 
  ([scanner] (add-string scanner "")
  ([scanner string]
    (let [char (current-char scanner)]))))

(defn- add-word [scanner])

(defn- skip-comment [scanner]
  (if (= \newline (current-char scanner)) 
    (advance scanner)
    (recur (advance scanner))))

(defn- scan-token [scanner]
  (let [char (current-char scanner)
        scanner (advance scanner)
        next (current-char scanner)
        ]
    (case char
      ;; one-character tokens
      \( (add-token scanner ::token/lparen)
      \) (add-token scanner ::token/rparen)
      \{ (add-token scanner ::token/lbrace)
      \} (add-token scanner ::token/rbrace)
      \[ (add-token scanner ::token/lbracket)
      \] (add-token scanner ::token/rbracket)
      \; (add-token scanner ::token/semicolon)
      \, (add-token scanner ::token/comma)
      \newline (add-token scanner ::token/newline)

      ;; two-character tokens
      ;; ->
      \- (if (= next \>)
           (add-token (advance scanner) ::token/rarrow)
           (add-error scanner (str "Expected ->. Got " char next)))
      ;; <-
      \< (if (= next \-)
           (add-token (advance scanner) ::token/larrow)
           (add-error scanner (str "Expected <-. Got " char next)))

      ;; |>
      \| (if (= next \>)
          (add-token (advance scanner) ::token/pipeline)
          (add-error scanner (str "Expected |>. Got " char next)))

      ;; possible additional operator: => (bind)

      ;; hashmap #{
      \# (if (= next \{)
           (add-token (advance scanner) ::token/starthash)
           (add-error scanner (str "Expected beginning of hash: #{. Got " char next)))

      ;; set ${
      \$ (if (= next \{)
           (add-token (advance scanner) ::token/startset)
           (add-error scanner (str "Expected beginning of set: ${. Got " char next)))

      ;; placeholder
      \_ (if (terminates? next)
          (add-token scanner ::token/placeholder)
          (add-word scanner))

      ;; comments
      ;; &
      \& (skip-comment scanner)

      ;; keywords
      \: (add-keyword scanner)

      ;; strings
      \" (add-string scanner)

      ;; word matches
      (cond
         (whitespace? char) scanner
         ;; (digit? char) (add-number scanner)
         ;; (alpha? char) (add-word scanner)
         :else (add-error scanner (str "Unexpected character: " char))))))

(defn- next-token [scanner]
  (assoc scanner ::start (::current scanner)))

(defn scan [source]
  (loop [scanner (new-scanner source)]
    (if (at-end? scanner)
      (let [scanner (add-token scanner ::eof)]
        {:tokens (::tokens scanner) 
          :errors (::errors scanner)})
      (recur (-> scanner (scan-token) (next-token))))))


(let [source "|)"]
  (scan source))

