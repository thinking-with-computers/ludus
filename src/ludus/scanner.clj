(ns ludus.scanner
  (:require [ludus.token :as token]))

(def reserved-words
  "List of Ludus reserved words."
  #{"fn"
    "if"
    "then"
    "else"
    "nil"
    "match"
    "with"
    "true"
    "false"
    "as"
    "ref"
    "swap"})
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
  (nth (::source scanner) (::current scanner)))

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

;; Note that commas are whitespace in Ludus
(defn- whitespace? [c]
  (or (= c \space) (= c \tab) (= c \return) (= c \,)))


(def terminators #{\: \; \newline \{ \( \[ \$ \# \- \< \&})

(defn- terminates? [c]
  (or (whitespace? c) (contains? terminators c)))

(defn- add-token
  ([scanner token-type]
   (add-token scanner token-type nil))
  ([scanner token-type literal]
   (update scanner ::tokens conj (token/token token-type (current-lexeme scanner) literal (::line scanner)))))

(defn- add-error [scanner msg])

(defn- scan-keyword 
  ([scanner] (scan-keyword scanner scanner))
  ([start current])
  )

(defn- add-keyword [scanner]
  (let [advanced (advance scanner)
        char (current-char advanced)]
    (if (not (alpha? char))
      (add-error scanner (str "Keywords must start with a letter, e.g. :foo. Got " \: char))
     (scan-keyword advanced))))

(defn- add-number [scanner])

(defn- add-string [scanner])

(defn- add-comment [scanner])

(defn- add-word [scanner])

(defn- scan-token [scanner]
  (let [char (current-char scanner)
        scanner (advance scanner)
        next (next-char scanner)]
    (case char
      ;; one-character tokens
      \( (add-token scanner ::token/lparen)
      \) (add-token scanner ::token/rparen)
      \{ (add-token scanner ::token/lbrace)
      \} (add-token scanner ::token/rbrace)
      \[ (add-token scanner ::token/lbracket)
      \] (add-token scanner ::token/rbracket)
      \; (add-token scanner ::token/semicolon)
      \_ (add-token scanner ::token/placeholder)
      \newline (add-token scanner ::token/newline)

      ;; two-character tokens
      ;; ->
      \- (if (= next \>)
           (add-token (advance (advance scanner)) ::token/rarrow)
           (add-error scanner (str "Expected ->. Got " char next)))
      ;; <-
      \< (if (= next \-)
           (add-token (advance (advance scanner)) ::token/larrow)
           (add-error scanner (str "Expected <-. Got " char next)))

      ;; begin hashmap #{
      \# (if (= next \{)
           (add-token (advance (advance scanner)) ::token/starthash)
           (add-error scanner (str "Expected beginning of hash: #{. Got " char next)))
      ;; begin set ${
      \$ (if (= next \{)
           (add-token (advance (advance scanner)) ::token/startset)
           (add-error scanner (str "Expected beginning of set: ${. Got " char next)))

      ;; comments
      ;; &
      \& (add-comment scanner)

      ;; keywords
      \: (add-keyword scanner)

      ;; strings
      \" (add-string scanner)

      ;; word matches
      (comment (cond
                 (digit? char) (add-number scanner)
                 (alpha? char) (add-word scanner)
                 :else (add-error scanner (str "Unexpected character: " char)))))))

(defn- next-token [scanner]
  (assoc scanner ::start (::current scanner)))

(defn scan [source]
  (loop [scanner (new-scanner source)]
    (if (at-end? scanner)
      (let [scanner (add-token scanner ::eof)]
        [(::tokens scanner) (::errors scanner)])
      (recur (-> scanner (scan-token) (next-token))))))