(ns ludus.grammar
 	(:require [ludus.parser-new :refer :all]
  		[ludus.scanner :as scan]))

(declare expression pattern)

(def separator (choice :separator [:comma :newline :break]))

(def separators (quiet (one+ separator)))

(def terminator (choice :terminator [:newline :semicolon :break]))

(def terminators (quiet (one+ terminator)))

(def nls? (quiet (zero+ :nls :newline)))

(def splat (group (order-1 :splat [(quiet :splat) :word])))

(def splattern (group (order-1 :splat [(quiet :splat) (flat (choice :splatted [:word :ignored :placeholder]))])))

(def literal (flat (choice :literal [:nil :true :false :number :string])))

(def tuple-pattern-term (flat (choice :tuple-pattern-term [pattern splattern])))

(def tuple-pattern-entry (weak-order :tuple-pattern-entry [tuple-pattern-term separators]))

(def tuple-pattern (group (order-1 :tuple-pattern
                           	[(quiet :lparen)
                           		(quiet (zero+ separator))
                           		(zero+ tuple-pattern-entry)
                           		(quiet :rparen)])))

(def list-pattern (group (order-1 :list-pattern 
                          	[(quiet :lbracket)
                           	(quiet (zero+ separator))
                           	(zero+ tuple-pattern-entry)
                           	(quiet :rbracket)])))

(def pair-pattern (order-0 :pair-pattern [:keyword pattern]))

(def dict-pattern-term (flat (choice :dict-pattern-term [pair-pattern :word splattern])))

(def dict-pattern-entry (weak-order :dict-pattern-entry [dict-pattern-term separators]))

(def dict-pattern (group (order-1 :dict-pattern 
                          	[(quiet :startdict)
                          	 (quiet (zero+ separator))
                          	 (zero+ dict-pattern-entry)
                          	 (quiet :rbrace)
                           	])))

(def struct-pattern (group (order-1 :struct-pattern
                            	[(quiet :startstruct)
                             	(quiet (zero+ separator))
                             	(zero+ dict-pattern-entry)
                             	(quiet :rbrace)
                             	])))

(def constraint (order-0 :constraint [(quiet :when) expression]))

(def typed (group (weak-order :typed [:word (quiet :as) :keyword])))

(def pattern (flat (choice :pattern [literal :ignored :placeholder typed :word :keyword tuple-pattern dict-pattern struct-pattern list-pattern])))

(def match-clause (group (weak-order :match-clause 
                          	[pattern (maybe constraint) (quiet :rarrow) expression])))

(def match-entry (weak-order :match-entry [match-clause terminators]))

(def match (group (order-1 :match 
                   	[(quiet :match) expression nls? 
                    	(quiet :with) (quiet :lbrace)
                    	(quiet (zero+ terminator))
                    	(one+ match-entry)
                    	(quiet :rbrace)
                    	])))

(def iff (group (order-1 :if [(quiet :if) 
                              nls? 
                              expression 
                              nls? 
                              (quiet :then) 
                              expression 
                              nls? 
                              (quiet :else) 
                              expression])))

(def cond-lhs (flat (choice :cond-lhs [expression :placeholder :else])))

(def cond-clause (group (weak-order :cond-clause [cond-lhs (quiet :rarrow) expression])))

(def cond-entry (weak-order :cond-entry [cond-clause terminators]))

(def condd (group (order-1 :cond [(quiet :cond) (quiet :lbrace)
                                  (quiet (zero+ terminator))
                                  (one+ cond-entry)
                                  (quiet :rbrace)])))

(def lett (group (order-1 :let [(quiet :let)
                               	pattern
                               	(quiet :equals)
                               	nls?
                               	expression])))

(def tuple-entry (weak-order :tuple-entry [expression separators]))
 
(def tuple (group (order-1 :tuple [(quiet :lparen)
                                 		(quiet (zero+ separator))
                                 		(zero+ tuple-entry)
                                 		(quiet :rparen)])))

(def list-term (flat (choice :list-term [splat expression])))

(def list-entry (order-1 :list-entry [list-term separators]))

(def listt (group (order-1 :list
                   	[(quiet :lbracket)
                   		(quiet (zero+ separator))
                   		(zero+ list-entry)
                   		(quiet :rbracket)])))

(def sett (group (order-1 :set [
                               	(quiet :startset)
                               	(quiet (zero+ separator))
                               	(zero+ list-entry)
                               	(quiet :rbrace)])))

(def pair (group (order-0 :pair [:keyword expression])))

(def struct-term (flat (choice :struct-term [:word pair])))

(def struct-entry (order-1 :struct-entry [struct-term separators]))

(def structt (group (order-1 :struct 
                     	[(quiet :startstruct)
                      	(quiet (zero+ separator))
                      	(zero+ struct-entry)
                      	(quiet :rbrace)])))

(def dict-term (flat (choice :dict-term [:word pair splat])))

(def dict-entry (order-1 :dict-entry [dict-term separators]))

(def dict (group (order-1 :dict
                  	[(quiet :startdict)
                   	(quiet (zero+ separator))
                   	(zero+ dict-entry)
                   	(quiet :rbrace)])))

(def arg-expr (flat (choice :arg-expr [:placeholder expression])))

(def arg-entry (weak-order :arg-entry [arg-expr separators]))

(def args (group (order-1 :args
                   [(quiet :lparen)
                    (quiet (zero+ separator))
                    (zero+ arg-entry)
                    (quiet :rparen)])))

(def synth-root (flat (choice :synth-root [:keyword :word :recur])))

(def synth-term (flat (choice :synth-term [args :keyword])))

(def synthetic (group (order-1 :synthetic [synth-root (zero+ synth-term)])))

(def fn-clause (group (order-0 :fn-clause [tuple-pattern (maybe constraint) (quiet :rarrow) expression])))

(def fn-entry (order-1 :fn-entry [fn-clause terminators]))

(def compound (group (order-1 :compound [(quiet :lbrace)
                                       		nls?
                                        	(maybe :string)
                                        	(quiet (zero+ terminator))
                                        	(one+ fn-entry)
                                        	(quiet :rbrace)
                                        	])))

(def clauses (flat (choice :clauses [fn-clause compound])))

(def named (group (order-1 :named [:word clauses])))

(def body (flat (choice :body [fn-clause named])))

(def fnn (group (order-1 :fn [(quiet :fn) body])))

(def block-line (weak-order :block-line [expression terminators]))

(def block (group (order-1 :block [(quiet :lbrace) 
                                  	(quiet (zero+ terminator))
                                  	(one+ block-line)
                                  	(quiet :rbrace)])))

(def pipeline (order-0 :pipeline [nls? :pipeline]))

(def do-entry (order-0 :do-entry [pipeline expression]))

(def doo (group (order-1 :do [(quiet :do)
                             	expression
                             	;; should this be zero+?
                             	(one+ do-entry)
                             	])))

(def reff (group (order-1 :ref [(quiet :ref) :word (quiet :equals) expression])))

(def spawn (group (order-1 :spawn [(quiet :spawn) expression])))

(def receive (group (order-1 :receive 
                     	[(quiet :receive) (quiet :lbrace)
                      	(quiet (zero+ terminator))
                      	(one+ match-entry)
                      	(quiet :rbrace)
                      	])))

(def compound-loop (group (order-0 :compound-loop
                           	[(quiet :lbrace)
                           		(quiet (zero+ terminator))
                           		(one+ fn-entry)
                            	(quiet :rbrace)])))

(def loopp (group (order-1 :loop 
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

(def testt (group (order-1 :test [(quiet :test) :string expression])))

(def importt (group (order-1 :import [(quiet :import) :string (quiet :as) :word])))

(def nss (group (order-1 :nss [(quiet :ns) 
                              	:word 
                              	(quiet :lbrace)
                              	(quiet (zero+ separator))
                              	(zero+ struct-entry)
                              	(quiet :rbrace)])))

(def toplevel (flat (choice :toplevel [importt nss expression testt])))

(def script-line (weak-order :script-line [toplevel terminators]))

(def script (order-0 :script [nls?
                              (one+ script-line)
                              (quiet :eof)]))


;;; REPL

(comment (def source 
           "if 1 then 2 else 3"
           )

  (def result (apply-parser script source))  


  (defn report [node] 
    (when (fail? node) (err-msg node))  
    node)   

  (defn clean [node]  
    (if (map? node) 
      (-> node    
        (report)    
        (dissoc     
          ;:status     
          :remaining  
          :token) 
        (update :data #(into [] (map clean) %)))    
      node))  

  (defn tap [x] (println "\n\n\n\n******NEW RUN\n\n:::=> " x "\n\n") x)   

  (def my-data (-> result     
                 clean   
                 tap 
                 ))

  (println my-data))