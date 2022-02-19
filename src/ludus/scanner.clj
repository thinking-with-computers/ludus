(ns ludus.scanner
  (:require 
    [ludus.token :as token]
    [clojure.pprint :as pp]
    [clojure.edn :as edn]
    [clojure.string :as s]))

(def reserved-words
  "List of Ludus reserved words."
  ;; see ludus-spec repo for more info
  {
   "as" ::token/as
   "cond" ::token/cond
   "else" ::token/else
   "false" ::token/false
   "fn" ::token/fn
   "if" ::token/if
   "let" ::token/let
   "match" ::token/match
   "mut" ::token/mut
   "nil" ::token/nil
   "panic!" ::token/panic
   "then" ::token/then
   "true" ::token/true
   "var" ::token/var
   "with" ::token/with
   ;; below here, probable
   "defer" ::token/defer
   "gen" ::token/gen
   "loop" ::token/loop
   "ns" ::token/ns
   "recur" ::token/recur
   "repeat" ::token/repeat
   "test" ::token/test
   "wait" ::token/wait
   "yield" ::token/yield
   ;; below here, possible
   "when" ::token/when
   })


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

;; for now, use very basic ASCII charset in words
;; TODO: research the implications of using the whole 
;; (defn- alpha? [c] (boolean (re-find #"\p{L}" (str c))))
(defn- alpha? [c]
  (or (char-in-range? \a \z c) (char-in-range? \A \Z c)))


;; legal characters in words
(def word-chars #{\_ \? \! \* \/})

(defn- word-char? [c]
  (or (alpha? c) (digit? c) (contains? word-chars c)))

(defn- whitespace? [c]
  (or (= c \space) (= c \tab)))

(def terminators #{\: \; \newline \{ \} \( \) \[ \] \$ \# \- \< \& \, \| nil \\})

(defn- terminates? [c]
  (or (whitespace? c) (contains? terminators c)))

(defn- add-token
  ([scanner token-type]
   (add-token scanner token-type nil))
  ([scanner token-type literal]
   (update scanner ::tokens conj 
     (token/token
       token-type 
       (current-lexeme scanner) 
       literal 
       (::line scanner) 
       (::start scanner)))))

;; TODO: errors should also be in the vector of tokens
;; The goal is to be able to be able to hand this to an LSP?
;; Do we need a different structure
(defn- add-error [scanner msg]
  (let [
        token (token/token 
                ::token/error
                (current-lexeme scanner)
                nil
                (::line scanner)
                (::start scanner))
        err-token (assoc token :msg msg)
        ]
    (-> scanner
      (update ::errors conj err-token)
      (update ::tokens conj err-token))))

(defn- add-keyword 
  [scanner]
  (loop [scanner scanner
         key ""]
    (let [char (current-char scanner)]
      (cond
        (terminates? char) (add-token scanner ::token/keyword (keyword key))
        (word-char? char) (recur (advance scanner) (str key char))
        :else (add-error scanner (str "Unexpected " char "after keyword :" key))))))

;; TODO: improve number parsing?
;; Currently this uses Clojure's number formatting rules (since we use the EDN reader)
;; These rules are here: https://cljs.github.io/api/syntax/number
(defn- add-number [char scanner]
  (loop [scanner scanner
         num (str char)
         float? false]
    (let [curr (current-char scanner)]
      (cond
        (= curr \_) (recur (advance scanner) num float?) ;; consume underscores unharmed
        (= curr \.) (if float?
                      (add-error scanner (str "Unexpected second decimal point after " num "."))
                      (recur (advance scanner) (str num curr) true))
        (terminates? curr) (add-token scanner ::token/number (edn/read-string num))
        (digit? curr) (recur (advance scanner) (str num curr) float?)
        :else (add-error scanner (str "Unexpected " curr " after number " num "."))))))

;; TODO: add string interpolation
;; This still has to be devised
(defn- add-string 
  [scanner]
  (loop [scanner scanner
         string ""]
    (let [char (current-char scanner)]
      (case char
        \newline (add-error scanner "Unterminated string.")
        \" (add-token (advance scanner) ::token/string string)
        \\ (let [next (next-char scanner)
                 scanner (if (= next \newline)
                           (update scanner ::line inc)
                           scanner)]
             (recur (advance (advance scanner)) (str string next)))
        (if (at-end? scanner)
          (add-error scanner "Unterminated string.")
          (recur (advance scanner) (str string char)))))))

(defn- add-word 
  [char scanner]
  (loop [scanner scanner
         word (str char)]
    (let [curr (current-char scanner)]
      (cond
        (terminates? curr) (add-token scanner (get reserved-words word ::token/word))
        (word-char? curr) (recur (advance scanner) (str word curr))
        :else (add-error scanner (str "Unexpected " curr " after word " word "."))))))

(defn- add-ignored
  [scanner]
  (loop [scanner scanner
         ignored "_"]
    (let [char (current-char scanner)]
      (cond
        (terminates? char) (add-token scanner ::token/ignored)
        (word-char? char) (recur (advance scanner) (str ignored char))
        :else (add-error scanner (str "Unexpected " char " after word " ignored "."))))))

(defn- add-comment [char scanner]
  (loop [scanner scanner
         comm (str char)]
    (let [char (current-char scanner)]
      (if (= \newline char)
        (if (s/starts-with? comm "&&&")
          (add-token (update scanner ::line inc) ::token/docstring)
          (add-token (update scanner ::line inc) ::token/comment))
        (recur (advance scanner) (str comm char))))))

(defn- scan-token [scanner]
  (let [char (current-char scanner)
        scanner (advance scanner)
        next (current-char scanner)]
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
      \newline (add-token (update scanner ::line inc) ::token/newline)
      \\ (add-token scanner ::token/backslash)
      \= (add-token scanner ::token/equals)

      ;; two-character tokens
      ;; ->
      \- (cond
           (= next \>) (add-token (advance scanner) ::token/rarrow)
           (digit? next) (add-number char scanner)
           :else (add-error scanner (str "Expected -> or negative number. Got " char next)))

      ;; at current we're not using this
      ;; <-
      \< (if (= next \-)
           (add-token (advance scanner) ::token/larrow)
           (add-error scanner (str "Expected <-. Got " char next)))

      ;; |>
      \| (if (= next \>)
           (add-token (advance scanner) ::token/pipeline)
           (add-error scanner (str "Expected |>. Got " char next)))

      ;; possible additional operator: bind/result
      ;; possible additional operator: bind/some
      ;; oh god, monads
      ;; additional arrow possibilities: >> ||> ~> => !>

      ;; hashmap #{
      \# (if (= next \{)
           (add-token (advance scanner) ::token/starthash)
           (add-error scanner (str "Expected beginning of hash: #{. Got " char next)))

      ;; set ${
      \$ (if (= next \{)
           (add-token (advance scanner) ::token/startset)
           (add-error scanner (str "Expected beginning of set: ${. Got " char next)))

      ;; placeholders
      ;; there's a flat _, and then ignored words
      \_ (cond 
           (terminates? next) (add-token scanner ::token/placeholder)
           (alpha? next) (add-ignored scanner)
           :else (add-error scanner (str "Expected placeholder: _. Got " char next)))

      ;; comments
      ;; & starts an inline comment
      ;; TODO: include comments in scanned file
      ;; TODO: add doc comments: &&&
      \& (add-comment char scanner)

      ;; keywords
      ;; TODO: instead of a separate token, scan a whole type keyword
      ;;    e.g. ::string, ::number
      \: (cond
           ;;(= \: next) (add-token (advance scanner) ::token/doublecolon))
           (alpha? next) (add-keyword scanner)
           :else (add-error scanner (str "Expected keyword. Got " char next)))

      ;; splats
      \. (let [after_next (current-char (advance scanner))]
           (if (= ".." (str next after_next))
             (add-token (advance (advance scanner)) ::token/splat)
             (add-error scanner (str "Expected splat: ... . Got " (str "." next after_next)))))

      ;; strings
      \" (add-string scanner)

      ;; word matches
      (cond
        ;; for now, don't add whitespace tokens
        (digit? char) (add-number char scanner)
        (alpha? char) (add-word char scanner)
        :else (add-error scanner (str "Unexpected character: " char))))))

(defn- next-token [scanner]
  (assoc scanner ::start (::current scanner)))

(defn scan [source]
  (loop [scanner (new-scanner source)]
    (if (at-end? scanner)
      (let [scanner (add-token scanner ::token/eof)]
        {:tokens (::tokens scanner) 
         :errors (::errors scanner)})
      (recur (-> scanner (scan-token) (next-token))))))
