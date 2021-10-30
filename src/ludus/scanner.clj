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
    "swap"
    ;; other possibilities
    ;; "pattern" -- first class patterns?
    })

(defn- new-scanner [source]
  {::source source
   ::length (count source)
   ::errors []
   ::start 0
   ::current 0
   ::line 1
   ::tokens []})

(defn- at-end? [scanner]
  (>= (::current scanner) (::length scanner)))

(defn- current-char [scanner]
  (nth (::source scanner) (::current scanner)))

(defn- advance [scanner]
  (update scanner ::current inc))

(defn- current-lexeme [scanner]
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

(defn- terminates? [c]
  (or (whitespace? c) (= c \:) (= c \newline)))

(defn- add-token
  ([scanner token-type]
   (add-token scanner token-type nil))
  ([scanner token-type literal]
   (update scanner ::tokens conj (token/token token-type (current-lexeme scanner) literal (::line scanner)))))

(defn- add-number [scanner])

(defn- add-string [scanner])

(defn- scan-token [scanner]
  (let [char (current-char scanner)
        scanner (advance scanner)]
    (case char
      ;; one-character tokens
      \( (add-token scanner ::token/lparen)
      \) (add-token scanner ::token/rparen)
      \{ (add-token scanner ::token/lbrace)
      \} (add-token scanner ::token/rbrace)
      \[ (add-token scanner ::token/lbracket)
      \] (add-token scanner ::token/rbracket)
      \; (add-token scanner ::token/semicolon)
      \newline (add-token scanner ::token/newline)
      ;; two-character tokens
      ;; ->
      ;; <-
      ;; //
      ;; begin hashmap #{
      ;; begin set ${

      ;; keywords
      ;;\: (add-keyword scanner)

      ;; strings
      ;;\"

      ;; word matches
      (comment (cond
                 (digit? char) (add-number scanner)
                 (alpha? char) (add-word scanner)
                 (= \_ char) (add-placeholder scanner)
                 :else (add-error scanner (str "Unexpected character: " char)))))))

(defn- next-token [scanner]
  (assoc scanner ::start (::current scanner)))

(defn scan [source]
  (loop [scanner (new-scanner source)]
    (if (at-end? scanner)
      (let [scanner (add-token scanner ::eof)]
        [(::tokens scanner) (::errors scanner)])
      (recur (-> scanner (scan-token) (next-token))))))