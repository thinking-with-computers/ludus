(ns ludus.grammar
 	(:require [ludus.parser-new :refer :all]
  		[ludus.scanner :as scan]))

(declare expression pattern)

(def separator (choice :separator [:comma :newline]))

(def terminator (choice :terminator [:newline :semicolon]))

(defn entries [name sep parser]
 	(zero+ (weak (order name [(quiet (one+ sep)) parser]))))

(def nls? (quiet (zero+ :nls :newline)))

(def splat (group (order :splat [(quiet :splat) :word])))

(def splattern (group (order :splat [(quiet :splat) (flat (choice :splatted [:word :ignored :placeholder]))])))

(def literal (flat (choice :literal [:nil :true :false :number :string])))

(def tuple-pattern-term (choice :tuple-pattern-term [pattern splattern]))

(def tuple-pattern-entries (entries :tuple-pattern-enries separator pattern))

(def tuple-pattern (group (order :tuple-pattern
                           	[(quiet :lparen)
                           		(quiet (zero+ separator))
                           		(maybe pattern)
                           		tuple-pattern-entries
                           		(quiet (zero+ separator))
                           		(quiet :rparen)])))

(def list-pattern (group (order :list-pattern 
                          	[(quiet :lbracket)
                           	(quiet (zero+ separator))
                           	(maybe pattern)
                           	tuple-pattern-entries
                           	(quiet (zero+ separator))
                           	(quiet :rbracket)])))

(def pair-pattern (order :pair-pattern [:keyword pattern]))

(def dict-pattern-term (flat (choice :dict-pattern-term [pair-pattern :word splattern])))

(def dict-pattern-entries (entries :dict-pattern-entries separator dict-pattern-term))

(def dict-pattern (group (order :dict-pattern 
                          	[(quiet :startdict)
                          	 (quiet (zero+ separator))
                          	 (maybe dict-pattern-term)
                          	 dict-pattern-entries
                          	 (quiet (zero+ separator))
                          	 (quiet :rbrace)
                           	])))

(def struct-pattern (group (order :struct-pattern
                            	[(quiet :startstruct)
                             	(quiet (zero+ separator))
                             	(maybe dict-pattern-term)
                             	dict-pattern-entries
                             	(quiet (zero+ separator))
                             	(quiet :rbrace)
                             	])))

(def constraint (order :constraint [:when expression]))

(def pattern (choice :pattern [literal :ignored :placeholder :word :keyword tuple-pattern dict-pattern struct-pattern list-pattern]))

(def match-clause (group (order :match-clause 
                          	[pattern (maybe constraint) (quiet :rarrow) expression])))

(def match-entries (entries :match-entries terminator match-clause))

(def match (group (order :match 
                   	[(quiet :match) expression nls? 
                    	(quiet :with) (quiet :lbrace) nls?
                    	match-clause
                    	match-entries
                    	nls?
                    	(quiet :rbrace)
                    	])))

(def iff (order :if [(quiet :if) 
                    	nls? 
                    	expression 
                    	nls? 
                    	(quiet :then) 
                    	expression 
                    	nls? 
                    	(quiet :else) 
                    	expression]))

(def cond-lhs (flat (choice :cond-lhs [expression :placeholder :else])))

(def cond-clause (group (order :cond-clause [cond-lhs (quiet :rarrow) expression])))

(def cond-entries (entries :cond-entries terminator cond-clause))

(def condd (order :cond [(quiet :cond) (quiet :lbrace)
                        	(quiet (zero+ terminator))
                        	cond-clause
                        	cond-entries
                        	(quiet (zero+ terminator))
                        	(quiet :rbrace)]))

(def lett (group (order :let [(quiet :let)
                             	pattern
                             	(quiet :equals)
                             	nls?
                             	expression])))

(def tuple-entry (weak (order :tuple-entry [(quiet (one+ separator)) expression])))

(def tuple-entries (entries :tuple-entries separator expression))

(def tuple (group (order :tuple 
                   	[(quiet :lparen)
                   		(quiet (zero+ separator))
                   		(maybe expression)
                   		tuple-entries
                   		(quiet (zero+ separator))
                   		(quiet :rparen)])))

(def list-term (flat (choice :list-term [splat expression])))

(def list-entry (weak (order :list-entry [(quiet (one+ separator)) list-term])))

(def list-entries (entries :list-entries separator list-term))

(def listt (group (order :list
                   	[(quiet :lbracket)
                   		(quiet (zero+ separator))
                   		(maybe list-term)
                   		list-entries
                   		(quiet (zero+ separator))
                   		(quiet :rbracket)])))

(def sett (group (order :set [
                             	(quiet :startset)
                             	(quiet (zero+ separator))
                             	(maybe list-term)
                             	list-entries
                             	(quiet (zero+ separator))
                             	(quiet :rbrace)])))

(def pair (group (order :pair [:keyword expression])))

(def struct-term (flat (choice :struct-term [:word pair])))

(def struct-entry (weak (order :struc-entry [(quiet (one+ separator)) struct-term])))

(def struct-entries (entries :struct-entries separator struct-term))

(def structt (group (order :struct 
                     	[(quiet :startstruct)
                      	(quiet (zero+ separator))
                      	(maybe struct-term)
                      	struct-entries
                      	(quiet (zero+ separator))
                      	(quiet :rbrace)])))

(def dict-term (flat (choice :dict-term [:word pair splat])))

(def dict-entry (weak (order :dict-entry [(quiet (one+ separator)) dict-term])))

(def dict-entries (entries :dict-entries separator dict-term))

(def dict (group (order :dict
                  	[(quiet :startdict)
                   	(quiet (zero+ separator))
                   	(maybe dict-term)
                   	dict-entries
                   	(quiet (zero+ separator))
                   	(quiet :rbrace)])))

(def arg-expr (flat (choice :arg-expr [:placeholder expression])))

(def arg-entry (weak (order :arg-entry [(quiet (one+ separator)) arg-expr])))

(def arg-entries (entries :arg-entries separator arg-expr))

(def arg-tuple (order :arg-tuple
                	[(quiet :lparen)
                		(quiet (zero+ separator))
                		(maybe arg-expr)
                		arg-entries
                		(quiet (zero+ separator))
                		(quiet :rparen)]))

(def synth-root (choice :synth-root [:keyword :word :recur]))

(def synth-term (choice :synth-term [arg-tuple :keyword]))

(def synthetic (order :synthetic [synth-root (zero+ synth-term)]))

(def fn-clause (group (order :fn-clause [tuple-pattern (maybe constraint) (quiet :rarrow) expression])))

(def fn-entry (weak (order :fn-entry [(quiet (one+ terminator)) fn-clause])))

(def fn-entries (entries :fn-entries terminator fn-clause))

(def compound (group (order :compound [(quiet :lbrace)
                             										nls?
                                      	(maybe :string)
                                      	nls?
                                      	fn-clause
                                      	fn-entries
                                      	nls?
                                      	(quiet :rbrace)
                                      	])))

(def clauses (flat (choice :clauses [compound fn-clause])))

(def named (group (order :named [:word clauses])))

(def body (flat (choice :body [fn-clause named])))

(def fnn (group (order :fn [(quiet :fn) body])))

(def block-lines (entries :block-lines terminator expression))

(def block (group (order :block [(quiet :lbrace) 
                                	nls? 
                                	expression 
                                	block-lines 
                                	nls? (quiet :rbrace)])))

(def pipeline (order :pipeline [nls? :pipeline]))

(def do-entry (weak (order :do-entry [pipeline expression])))

(def doo (group (order :do [
                           	(quiet :do)
                           	expression
                           	(one+ do-entry)
                           	])))

(def reff (group (order :ref [(quiet :ref) :word (quiet :equals) expression])))

(def spawn (group (order :spawn [(quiet :spawn) expression])))

(def receive (group (order :receive 
                     	[(quiet :receive) (quiet :lbrace) nls?
                      	match-clause
                      	match-entries
                      	nls?
                      	(quiet :rbrace)
                      	])))

(def compound-loop (group (order :compound-loop
                           	[(quiet :lbrace)
                            	nls?
                            	fn-clause
                            	fn-entries
                            	nls?
                            	(quiet :rbrace)])))

(def loopp (group (order :loop 
                   	[(quiet :loop) tuple (quiet :with)
                    	(flat (choice :loop-body [fn-clause compound-loop]))])))

(def expression (flat (choice :expression [fnn 
                                          	match
                                          	loopp
                                          	lett 
                                          	iff 
                                          	condd
                                          	spawn
                                          	receive
                                          	synthetic 
                                          	block 
                                          	doo
                                          	reff
                                          	structt 
                                          	dict
                                          	listt 
                                          	sett
                                          	tuple 
                                          	literal])))

(def test (group (order :test [(quiet :test) :string expression])))

(def importt (group (order :import [(quiet :import) :string (quiet :as) :word])))

(def nss (group (order :nss [(quiet :ns) 
                            	:word 
                            	(quiet :lbrace)
                            	(quiet (zero+ separator))
                            	(maybe struct-term)
                            	(zero+ struct-entry)
                            	(quiet (zero+ separator))
                            	(quiet :rbrace)])))

(def toplevel (flat (choice :toplevel [importt nss expression test])))

(def script-lines (entries :script-lines terminator toplevel))

(def script (order :script [nls? 
                           	toplevel
                           	script-lines
                            nls? 
                            (quiet :eof)]))


;;;;;;;;;;;;;;;; REPL CRUFT

;;TODO: improve current bug reporting in the parser
;; --e.g., give functions better names in the stack trace
;; --I think this might require a macro (::facepalm::)
;;TODO: fix forward declaration errors


(def eg (:tokens (scan/scan
                   "receive { _ -> 1; () -> 2 }
                    "
                   )))



(def result (apply-parser script eg))


(defn report [node]
 	(when (fail? node) (err-msg node))
 	node)

(defn clean [node]
 	(if (map? node)
  		(-> node
   			(report)
   			(dissoc 
    				:status 
    				:remaining 
    				:token)
   			(update :data #(into [] (map clean) %)))
  		node))

(defn tap [x] (println "\n\n\n\n******NEW PARSE\n\n:::=> " x "\n\n") x)

(def my-data (-> result 
              	clean 
              	tap
              	))

my-data