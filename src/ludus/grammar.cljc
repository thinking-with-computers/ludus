(ns ludus.grammar
 	(:require
    #?(
       :clj [ludus.parser :refer :all]
       :cljs [ludus.parser
              :refer [choice quiet one+ zero+ group order-0 order-1 flat maybe weak-order]
              :refer-macros [defp]
              ]
       )
    [ludus.scanner :as s]
    ))

(declare expression pattern binding non-binding simple)

(defp separator choice [:comma :newline :break])

(defp separators quiet one+ separator)

(defp terminator choice [:newline :semicolon :break])

(defp terminators quiet one+ terminator)

(defp nls? quiet zero+ :newline)

(defp splat group order-1 [(quiet :splat) :word])

(defp patt-splat-able flat choice [:word :ignored :placeholder])

(defp splattern group order-1 [(quiet :splat) (maybe patt-splat-able)])

(defp literal flat choice [:nil :true :false :number :string])

(defp tuple-pattern-term flat choice [pattern splattern])

(defp tuple-pattern-entry weak-order [tuple-pattern-term separators])

(defp tuple-pattern group order-1 [(quiet :lparen)
                                 		(quiet (zero+ separator))
                                 		(zero+ tuple-pattern-entry)
                                 		(quiet :rparen)])

(defp list-pattern group order-1 [(quiet :lbracket)
                                 	(quiet (zero+ separator))
                                 	(zero+ tuple-pattern-entry)
                                 	(quiet :rbracket)])

(defp pair-pattern group weak-order [:keyword pattern])

(defp typed group weak-order [:word (quiet :as) :keyword])

(defp dict-pattern-term flat choice [pair-pattern typed :word splattern])

(defp dict-pattern-entry weak-order [dict-pattern-term separators])

(defp dict-pattern group order-1 [(quiet :startdict)
                                	 (quiet (zero+ separator))
                                	 (zero+ dict-pattern-entry)
                                	 (quiet :rbrace)
                                 	])

(defp struct-pattern group order-1 [(quiet :startstruct)
                                   	(quiet (zero+ separator))
                                   	(zero+ dict-pattern-entry)
                                   	(quiet :rbrace)
                                   	])

(defp guard order-0 [(quiet :if) simple])

(defp pattern flat choice [literal 
                           :ignored 
                           :placeholder 
                           typed 
                           :word 
                           :keyword
                           :else 
                           tuple-pattern 
                           dict-pattern 
                           struct-pattern 
                           list-pattern])

(defp match-clause group weak-order [pattern (maybe guard) (quiet :rarrow) expression])

(defp match-entry weak-order [match-clause terminators])

(defp match group order-1 [(quiet :match) simple nls? 
                          	(quiet :with) (quiet :lbrace)
                          	(quiet (zero+ terminator))
                          	(one+ match-entry)
                          	(quiet :rbrace)
                          	])

(defp if-expr group order-1 [(quiet :if) 
                             nls? 
                             simple 
                             nls? 
                             (quiet :then) 
                             expression 
                             nls? 
                             (quiet :else) 
                             expression])

(defp when-lhs flat choice [simple :placeholder :else])

(defp when-clause group weak-order [when-lhs (quiet :rarrow) expression])

(defp when-entry weak-order [when-clause terminators])

(defp when-expr group order-1 [(quiet :when) (quiet :lbrace)
                               (quiet (zero+ terminator))
                               (one+ when-entry)
                               (quiet :rbrace)])

(defp let-expr group order-1 [(quiet :let)
                             	pattern
                             	(quiet :equals)
                             	nls?
                             	non-binding])

(defp tuple-entry weak-order [non-binding separators])
 
(defp tuple group order-1 [(quiet :lparen)
                         		(quiet (zero+ separator))
                         		(zero+ tuple-entry)
                         		(quiet :rparen)])

(defp list-term flat choice [splat non-binding])

(defp list-entry order-1 [list-term separators])

(defp list-literal group order-1 [(quiet :lbracket)
                                		(quiet (zero+ separator))
                                		(zero+ list-entry)
                                		(quiet :rbracket)])

(defp set-literal group order-1 [(quiet :startset)
                                	(quiet (zero+ separator))
                                	(zero+ list-entry)
                                	(quiet :rbrace)])

(defp pair group order-0 [:keyword non-binding])

(defp struct-term flat choice [:word pair])

(defp struct-entry order-1 [struct-term separators])

(defp struct-literal group order-1 [(quiet :startstruct)
                                   	(quiet (zero+ separator))
                                   	(zero+ struct-entry)
                                   	(quiet :rbrace)])

(defp dict-term flat choice [splat :word pair])

(defp dict-entry order-1 [dict-term separators])

(defp dict group order-1 [(quiet :startdict)
                         	(quiet (zero+ separator))
                         	(zero+ dict-entry)
                         	(quiet :rbrace)])

(defp arg-expr flat choice [:placeholder non-binding])

(defp arg-entry weak-order [arg-expr separators])

(defp args group order-1 [(quiet :lparen)
                          (quiet (zero+ separator))
                          (zero+ arg-entry)
                          (quiet :rparen)])

(defp recur-call group order-1 [(quiet :recur) tuple])

(defp synth-root flat choice [:keyword :word])

(defp synth-term flat choice [args :keyword])

(defp synthetic group order-1 [synth-root (zero+ synth-term)])

(defp fn-clause group order-1 [tuple-pattern (maybe guard) (quiet :rarrow) expression])

(defp fn-entry order-1 [fn-clause terminators])

(defp fn-compound group order-1 [(quiet :lbrace)
                               		nls?
                                	(maybe :string)
                                	(quiet (zero+ terminator))
                                	(one+ fn-entry)
                                	(quiet :rbrace)
                                	])

(defp clauses flat choice [fn-clause fn-compound])

(defp fn-named group order-1 [(quiet :fn) :word clauses])

(defp lambda group order-1 [(quiet :fn) fn-clause])

(defp block-line weak-order [expression terminators])

(defp block group order-1 [(quiet :lbrace) 
                          	(quiet (zero+ terminator))
                          	(one+ block-line)
                          	(quiet :rbrace)])

(defp pipeline quiet order-0 [nls? :pipeline])

(defp do-entry order-1 [pipeline expression])

(defp do-expr group order-1 [(quiet :do)
                            	expression
                            	(one+ do-entry)
                            	])

(defp ref-expr group order-1 [(quiet :ref) :word (quiet :equals) expression])

; (defp spawn group order-1 [(quiet :spawn) expression])

; (defp receive group order-1 [(quiet :receive) (quiet :lbrace)
;                             	(quiet (zero+ terminator))
;                             	(one+ match-entry)
;                             	(quiet :rbrace)
;                             	])

(defp compound-loop group order-0 [(quiet :lbrace)
                                 		(quiet (zero+ terminator))
                                 		(one+ fn-entry)
                                  	(quiet :rbrace)])

(defp loop-expr group order-1 [(quiet :loop) tuple (quiet :with)
                              	(flat (choice :loop-body [fn-clause compound-loop]))])

(defp collection flat choice [struct-literal dict list-literal set-literal tuple])

(defp simple flat choice [literal collection synthetic recur-call lambda])

(defp compound flat choice [match loop-expr if-expr when-expr do-expr block])

(defp binding flat choice [fn-named fn-compound let-expr ref-expr])

(defp non-binding flat choice [simple compound])

(defp expression flat choice [binding non-binding])

(defp test-expr group order-1 [(quiet :test) :string non-binding])

(defp import-expr group order-1 [(quiet :import) :string (quiet :as) :word])

(defp ns-expr group order-1 [(quiet :ns) 
                            	:word 
                            	(quiet :lbrace)
                            	(quiet (zero+ separator))
                            	(zero+ struct-entry)
                            	(quiet :rbrace)])

(defp use-expr group order-1 [(quiet :use :word)])

(defp toplevel flat choice [import-expr
                            ns-expr
                            expression 
                            test-expr
                            use-expr])

(defp script-line weak-order [toplevel terminators])

(defp script order-0 [nls?
                      (one+ script-line)
                      (quiet :eof)])
