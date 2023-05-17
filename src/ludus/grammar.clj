(ns ludus.grammar
 	(:require [ludus.parser-new :refer :all]
  		[ludus.scanner :as scan]))

(declare expression pattern)

(def separator (choice :separator [:comma :newline]))

(def terminator (choice :terminator [:newline :semicolon]))

(def nls? (quiet (zero+ :nls :newline)))

(def splat (group (order :splat [(quiet :splat) :word])))

(def splattern (group (order :splat [(quiet :splattern) (flat (choice :splatted [:word :ignored :placeholder]))])))

(def literal (flat (choice :literal [:nil :true :false :number :string])))

(def tuple-pat-term (choice :tuple-pat-term [pattern splattern]))

(def tuple-pat-entry (order :tuple-pat-enry [(quiet (one+ separator)) pattern]))

(def tuple-pat (group (order :tuple-pat
                       	[(quiet :lparen)
                       		(quiet (zero+ separator))
                       		(maybe pattern)
                       		(zero+ tuple-pat-entry)
                       		(quiet (zero+ separator))
                       		(quiet :rparen)])))

;; TODO: list, dict, struct patterns

(def pattern (choice :pattern [:literal :ignored :placeholder :word :keyword tuple-pat]))

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

(def cond-entry (order :cond-entry [(quiet (one+ terminator)) cond-clause]))

(def condd (order :cond [(quiet :cond) (quiet :lbrace)
                        	(quiet (zero+ terminator))
                        	cond-clause
                        	(zero+ cond-entry)
                        	(quiet (zero+ terminator))
                        	(quiet :rbrace)]))

(def lett (order :let [(quiet :let)
                      	pattern
                      	(quiet :equals)
                      	nls?
                      	expression]))

(def tuple-entry (order :tuple-entry [(quiet (one+ separator)) expression]))

(def tuple (order :tuple 
            	[(quiet :lparen)
            		(quiet (zero+ separator))
            		(maybe expression)
            		(zero+ tuple-entry)
            		(quiet (zero+ separator))
            		(quiet :rparen)]))

(def list-term (flat (choice :list-term [splat expression])))

(def list-entry (order :list-entry [(quiet (one+ separator)) list-term]))

(def listt (order :list
            	[(quiet :lbracket)
            		(quiet (zero+ separator))
            		(maybe list-term)
            		(zero+ list-entry)
            		(quiet (zero+ separator))
            		(quiet :rbracket)]))

(def pair (group (order :pair [:keyword expression])))

(def struct-term (flat (choice :struct-term [:word pair])))

(def struct-entry (order :struct-entry [(quiet (one+ separator)) struct-term]))

(def structt (order :struct 
              	[(quiet :startstruct)
               	(quiet (zero+ separator))
               	(maybe struct-term)
               	(zero+ struct-entry)
               	(quiet (zero+ separator))
               	(quiet :rbrace)]))

(def dict-term (flat (choice :dict-term [:word pair splat])))

(def dict-entry (order :dict-entry [(quiet (one+ separator)) dict-term]))

(def dict (order :dict
           	[(quiet :startdict)
            	(quiet (zero+ separator))
            	(maybe dict-term)
            	(zero+ dict-entry)
            	(quiet (zero+ separator))
            	(quiet :rbrace)]))

(def arg-expr (flat (choice :arg-expr [:placeholder expression])))

(def arg-entry (order :arg-entry [(quiet (one+ separator)) arg-expr]))

(def arg-tuple (order :arg-tuple
                	[(quiet :lparen)
                		(quiet (zero+ separator))
                		(maybe arg-expr)
                		(zero+ arg-entry)
                		(quiet (zero+ separator))
                		(quiet :rparen)]))

(def synth-root (choice :synth-root [:keyword :word :recur]))

(def synth-term (choice :synth-term [arg-tuple :keyword]))

(def synthetic (order :synthetic [synth-root (zero+ synth-term)]))

(def fn-clause (group (order :fn-clause [tuple-pat (quiet :rarrow) expression])))

(def fn-entry (order :fn-entry [(quiet (one+ terminator)) fn-clause]))

(def compound (group (order :compound [(quiet :lbrace)
                                      	(maybe :string)
                                      	fn-clause
                                      	(zero+ fn-entry)
                                      	nls?
                                      	(quiet :rbrace)
                                      	])))

(def clauses (flat (choice :clauses [compound fn-clause])))

(def named (group (order :named [:word clauses])))

(def body (flat (choice :body [fn-clause named])))

(def fnn (group (order :fn [(quiet :fn) body])))

(def block-line (order :block-line [(quiet terminator) expression]))

(def block (group (order :block [(quiet :lbrace) nls? expression (zero+ block-line) nls? (quiet :rbrace)])))

(def expression (flat (choice :expression [fnn lett iff condd synthetic block structt listt tuple literal])))

(def importt (group (order :import [(quiet :import) :string (quiet :as) :word])))

(def nss (group (order :nss [(quiet :ns) 
                            	:word 
                            	(quiet :lbrace)
                            	(quiet (zero+ separator))
                            	(maybe struct-term)
                            	(zero+ struct-entry)
                            	(quiet (zero+ separator))
                            	(quiet :rbrace)])))

(def toplevel (flat (choice :toplevel [importt nss expression])))

(def script-line (order :script-line [(quiet (one+ terminator)) toplevel]))

(def script (order :script [nls? toplevel (zero+ script-line) nls? (quiet :eof)]))


;;;;;;;;;;;;;;;; REPL CRUFT

(def eg (:tokens (scan/scan
                   "
add (1, 2)
fn foo { (_) -> (1, 2) }"
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

(def my-data (-> result clean tap))

my-data

(def my-first (-> my-data first))

(def my-sec (map :data (-> my-data second :data)))

(concat my-first my-sec)