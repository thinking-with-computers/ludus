(ns ludus.scanner
  (:require
    [ludus.token :as token]
    ;; [clojure.pprint :as pp]
    [clojure.edn :as edn]))

(def reserved-words
  "List of Ludus reserved words."
  ;; see ludus-spec repo for more info
  {"as" :as ;; impl for `import`; not yet for patterns
   ;"cond" :cond ;; impl
   "do" :do ;; impl
   "else" :else ;; impl
   "false" :false ;; impl -> literal word
   "fn" :fn ;; impl
   "if" :if ;; impl
   "import" :import ;; impl
   "let" :let ;; impl
   "loop" :loop ;; impl
   ; "match" :match ;; impl
   "nil" :nil ;; impl -> literal word
   "ns" :ns ;; impl
   ;; "panic!" :panic ;; impl (should be a function)
   "recur" :recur ;; impl
   "ref" :ref ;; impl
   "then" :then ;; impl
   "true" :true ;; impl -> literal word
   "with" :with ;; impl

   ;; actor model/concurrency
   "receive" :receive
   ;;"self" :self ;; maybe not necessary?: self() is a function
   ;;"send" :send ;; not necessary: send(pid, message) is a function
   "spawn" :spawn
   ;;"to" :to ;; not necessary if send is a function
   ;; type system
   ;; "data" :data ;; we are going to tear out datatypes for now: see if dynamism works for us
   ;; others
   ;;"repeat" :repeat ;; syntax sugar over "loop": still unclear what this syntax could be
   "test" :test
   "when" :when
   ;; "module" :module ;; not necessary if we don't have datatypes
   "is" :is
   })

(def literal-words {
                    "true" true
                    "false" false
                    "nil" nil
                    })

(defn- new-scanner
  "Creates a new scanner."
  [source]
  {:source source
   :length (count source)
   :errors []
   :start 0
   :current 0
   :line 1
   :tokens []})

(defn- at-end?
  "Tests if a scanner is at end of input."
  [scanner]
  (>= (:current scanner) (:length scanner)))

(defn- current-char
  "Gets the current character of the scanner."
  [scanner]
  (nth (:source scanner) (:current scanner) nil))

(defn- advance
  "Advances the scanner by a single character."
  [scanner]
  (update scanner :current inc))

(defn- next-char
  "Gets the next character from the scanner."
  [scanner]
  (current-char (advance scanner)))

(defn- current-lexeme
  [scanner]
  (subs (:source scanner) (:start scanner) (:current scanner)))

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

(defn- lower? [c] (char-in-range? \a \z c))

(defn- upper? [c] (char-in-range? \A \Z c))

;; legal characters in words
(def word-chars #{\_ \? \! \* \/})

(defn- word-char? [c]
  (or (alpha? c) (digit? c) (contains? word-chars c)))

(defn- whitespace? [c]
  (or (= c \space) (= c \tab)))

(def terminators #{\: \; \newline \{ \} \( \) \[ \] \$ \# \- \= \& \, \> nil \\})

(defn- terminates? [c]
  (or (whitespace? c) (contains? terminators c)))

(defn- add-token
  ([scanner token-type]
   (add-token scanner token-type nil))
  ([scanner token-type literal]
   (update scanner :tokens conj
     (token/token
       token-type
       (current-lexeme scanner)
       literal
       (:line scanner)
       (:start scanner)))))

;; TODO: errors should also be in the vector of tokens
;; The goal is to be able to be able to hand this to an LSP?
;; Do we need a different structure
(defn- add-error [scanner msg]
  (let [token (token/token
                :error
                (current-lexeme scanner)
                nil
                (:line scanner)
                (:start scanner))
        err-token (assoc token :message msg)]
    (-> scanner
      (update :errors conj err-token)
      (update :tokens conj err-token))))

(defn- add-keyword
  [scanner]
  (loop [scanner scanner
         key ""]
    (let [char (current-char scanner)]
      (cond
        (terminates? char) (add-token scanner :keyword (keyword key))
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
        (terminates? curr) (add-token scanner :number (edn/read-string num))
        (digit? curr) (recur (advance scanner) (str num curr) float?)
        :else (add-error scanner (str "Unexpected " curr " after number " num "."))))))

;; TODO: activate string interpolation
(defn- add-string
  [scanner]
  (loop [scanner scanner
         string ""
         interpolate? false]
    (let [char (current-char scanner)]
      (case char
        \{ (recur (update (advance scanner)) (str string char) true)
        ; allow multiline strings
        \newline (recur (update (advance scanner) :line inc) (str string char) interpolate?)
        \" (if interpolate?
             ;(add-token (advance scanner) :interpolated string)
             (add-token (advance scanner) :string string)
             (add-token (advance scanner) :string string))
        \\ (let [next (next-char scanner)
                 scanner (if (= next \newline)
                           (update scanner :line inc)
                           scanner)]
             (recur (advance (advance scanner)) (str string next) interpolate?))
        (if (at-end? scanner)
          (add-error scanner "Unterminated string.")
          (recur (advance scanner) (str string char) interpolate?))))))

(defn- add-word
  [char scanner]
  (loop [scanner scanner
         word (str char)]
    (let [curr (current-char scanner)]
      (cond
        (terminates? curr) (add-token scanner 
                             (get reserved-words word :word) 
                             (get literal-words word :none))
        (word-char? curr) (recur (advance scanner) (str word curr))
        :else (add-error scanner (str "Unexpected " curr " after word " word "."))))))

(defn- add-data
  [char scanner]
  (loop [scanner scanner
         word (str char)]
    (let [curr (current-char scanner)]
      (cond
        (terminates? curr) (add-token scanner :datatype)
        (word-char? curr) (recur (advance scanner) (str word curr))
        :else (add-error scanner (str "Unexpected " curr " after datatype " word "."))))))

(defn- add-ignored
  [scanner]
  (loop [scanner scanner
         ignored "_"]
    (let [char (current-char scanner)]
      (cond
        (terminates? char) (add-token scanner :ignored)
        (word-char? char) (recur (advance scanner) (str ignored char))
        :else (add-error scanner (str "Unexpected " char " after word " ignored "."))))))

(defn- add-comment [char scanner]
  (loop [scanner scanner
         comm (str char)]
    (let [char (current-char scanner)]
      (if (= \newline char)
        (update scanner :line inc)
        (recur (advance scanner) (str comm char))))))

(defn- scan-token [scanner]
  (let [char (current-char scanner)
        scanner (advance scanner)
        next (current-char scanner)]
    (case char
      ;; one-character tokens
      \( (add-token scanner :lparen)
      ;; :break is a special zero-char token before closing braces
      ;; it makes parsing much simpler
      \) (add-token (add-token scanner :break) :rparen)
      \{ (add-token scanner :lbrace)
      \} (add-token (add-token scanner :break) :rbrace)
      \[ (add-token scanner :lbracket)
      \] (add-token (add-token scanner :break) :rbracket)
      \; (add-token scanner :semicolon)
      \, (add-token scanner :comma)
      \newline (add-token (update scanner :line inc) :newline)
      \\ (add-token scanner :backslash)
      \= (add-token scanner :equals)
      \> (add-token scanner :pipeline)

      ;; two-character tokens
      ;; ->
      \- (cond
           (= next \>) (add-token (advance scanner) :rarrow)
           (digit? next) (add-number char scanner)
           :else (add-error scanner (str "Expected -> or negative number after `-`. Got `" char next "`")))

      ;; dict #{
      \# (if (= next \{)
           (add-token (advance scanner) :startdict)
           (add-error scanner (str "Expected beginning of dict: #{. Got " char next)))

      ;; set ${
      \$ (if (= next \{)
           (add-token (advance scanner) :startset)
           (add-error scanner (str "Expected beginning of set: ${. Got " char next)))

      ;; struct @{
      \@ (if (= next \{)
           (add-token (advance scanner) :startstruct)
           (add-error scanner (str "Expected beginning of struct: @{. Got " char next)))

      ;; placeholders
      ;; there's a flat _, and then ignored words
      \_ (cond
           (terminates? next) (add-token scanner :placeholder)
           (alpha? next) (add-ignored scanner)
           :else (add-error scanner (str "Expected placeholder: _. Got " char next)))

      ;; comments
      ;; & starts an inline comment
      \& (add-comment char scanner)

      ;; keywords
      \: (cond
           (alpha? next) (add-keyword scanner)
           :else (add-error scanner (str "Expected keyword. Got " char next)))

      ;; splats
      \. (let [after_next (current-char (advance scanner))]
           (if (= ".." (str next after_next))
             (add-token (advance (advance scanner)) :splat)
             (add-error scanner (str "Expected splat: ... . Got " (str "." next after_next)))))

      ;; strings
      \" (add-string scanner)

      ;; word matches
      (cond
        (whitespace? char) scanner ;; for now just skip whitespace characters
        (digit? char) (add-number char scanner)
        (upper? char) (add-word char scanner) ;; no datatypes for now
        (lower? char) (add-word char scanner)
        :else (add-error scanner (str "Unexpected character: " char))))))

(defn- next-token [scanner]
  (assoc scanner :start (:current scanner)))

(defn scan [source]
  (loop [scanner (new-scanner source)]
    (if (at-end? scanner)
      (let [scanner (add-token (add-token scanner :break) :eof)]
        {:tokens (:tokens scanner)
         :errors (:errors scanner)})
      (recur (-> scanner (scan-token) (next-token))))))

