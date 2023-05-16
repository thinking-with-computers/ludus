(ns ludus.parser-new
 	(:require
  		[ludus.scanner :as scan]))

(defn ? [val default] (if (nil? val) default val))

(defn ok? [{status :status}]
 	(= status :ok))

(def failing #{:err :none})

(def passing #{:ok :group :silent})

(defn pass? [{status :status}] (contains? passing status))

(defn fail? [{status :status}] (contains? failing status))

(defn data [{d :data}] d)

(defn remaining [{r :remaining}] r)

(defn pname [parser] (? (:name parser) parser))

(defn str-part [kw] (apply str (next (str kw))))

(defn kw+str [kw mystr] (keyword (str (str-part kw) mystr)))

(defn value [token]
 	(if (= :none (:literal token)) (:lexeme token) (:literal token)))

(defn apply-kw-parser [kw tokens]
 	(let [token (first tokens)]
  		;(println "applying kw parser " kw " to " token)
  		(if (= kw (:type token))
   			{:status :ok 
   				:type kw 
   				:data (if (value token) [(value token)] [])
   				:token token 
   				:remaining (rest tokens)}
   			{:status :none :token token :trace [kw] :remaining (rest tokens)})))

(defn apply-fn-parser [parser tokens]
 	(let [rule (:rule parser) name (:name parser)] 	
  		;(println "appying fn parser " name " to " (first tokens)) 
  		(rule tokens)))

(defn apply-parser [parser tokens]
 	(cond 
  		(keyword? parser) (apply-kw-parser parser tokens)
  		(:rule parser) (apply-fn-parser parser tokens)
  		:else (throw (Exception. "`apply-parser` requires a parser"))))

(defn choice [name parsers]
 	{:name name
 		:rule (fn choice-fn [tokens]
       				;(println "entering CHOICE" name)
         		(loop [ps parsers]
          			(let [result (apply-parser (first ps) tokens) 
               				rem-ts (remaining result)
               				rem-ps (rest ps)]
         						(cond
          							(pass? result) ;result
          							{:status :ok :type name :data [result] :token (first tokens) :remaining rem-ts}

          							(= :err (:status result))
          							(update result :trace #(conj % name))

          							(empty? rem-ps)
          							{:status :none :token (first tokens) :trace [name] :remaining rem-ts}

          							:else (recur rem-ps)))))})

(defn order [name parsers]
 	{:name name
 		:rule (fn order-fn [tokens] 
      					;(println "entering ORDER" name)
        			(let [origin (first tokens)
             				first-result (apply-parser (first parsers) tokens)]
          			(case (:status first-result)
           				(:err :none) 
           				{:status :none 
           					:token (first tokens) 
           					:trace [name]
           					:remaining tokens}

           				(:ok :quiet :group)
            			(loop [ps (rest parsers) 
                  				results (case (:status first-result)
                           					:ok [first-result]
                           					:quiet []
                           					:group (:data first-result)) 
                  				ts (remaining first-result)]
              			(let [result (apply-parser (first ps) ts)
                   				res-rem (remaining result)]
               				(if (empty? (rest ps)) 
               						(case (:status result)
                							:ok {:status :group 
                    								:type name 
                    								:data (conj results result) 
                    								:token origin 
                    								:remaining res-rem}
          							
                							:quiet {:status :group
                       								:type name 
                       								:data results 
                       								:token origin 
                       								:remaining res-rem}

                							:group {:status :group
                       								:type name
                       								:data (vec (concat results (:data result)))
                       								:token origin 
                       								:remaining res-rem}
          							
                							(:err :none)
                							(assoc (update result :trace #(conj % name)) :status :err))
             							
                					(case (:status result)  
                  					:ok 	(recur (rest ps) (conj results result) res-rem)
                  					:group	(recur (rest ps) 
                          						(vec (concat results (:data result)))
                          						res-rem)
                  					:quiet 	(recur (rest ps) results res-rem)
                  					(:err :none) 	
                  					(assoc (update result :trace #(conj % name)) :status :err))))))))})

(defn quiet [parser]
 	{:name (kw+str (? (:name parser) parser) "-quiet")
 		:rule (fn quiet-fn [tokens]
        			(let [result (apply-parser parser tokens)]
         				(if (pass? result) 
        							(assoc result :status :quiet)
          					result)))})

(defn zero+ 
 	([parser] (zero+ (pname parser) parser))
 	([name parser]
  	{:name (kw+str name "-zero+")
  		:rule (fn zero+fn [tokens]
       					;(println "entering ZERO+")
         			(loop [results [] 
              					ts tokens]
           			;(println "looping ZERO+" (? (:name parser) parser))
          				(let [result (apply-parser parser ts)]
           					(case (:status result)
            						:ok (recur (conj results result) (remaining result))
            						:group (recur (vec (concat results (:data result))) (remaining result))
            						:quiet (recur results (remaining result))
            						{:status :group :type name :data results :token (first tokens) :remaining ts}))))}))

(defn one+
 	([parser] (one+ (pname parser) parser))
 	([name parser]
 		{:name (kw+str name "-one+")
  		:rule (fn one+fn [tokens]
         			(let [first-result (apply-parser parser tokens)
              				rest-parser (zero+ name parser)]
          				(case (:status first-result)
           					(:ok :group)
           					(let [rest-result (apply-parser rest-parser (remaining first-result))]
            						{:status :group
            							:type name 
            							:data (vec (concat [first-result] (data rest-result)))
            							:token (first tokens)
            							:remaining (remaining rest-result)})
    						
          						:quiet
          						(let [rest-result (apply-parser rest-parser (remaining first-result))]
           							{:status :quiet
           								:type name
           								:data []
           								:token (first tokens)
           								:remaining (remaining rest-result)})

           					(:err :none) first-result)))}))

(defn maybe
 	([parser] (maybe (pname parser) parser))
 	([name parser]
 		{:name (kw+str name "-maybe")
 			:rule (fn maybe-fn [tokens]
        				(let [result (apply-parser parser tokens)]
         					(if (pass? result)
          						result
          						{:status :group :type name :data [] :token (first tokens) :remaining tokens}
          						)))}))

(defn flat
 	([parser] (flat (pname parser) parser))
 	([name parser]
 		{:name (kw+str name "-flat")
  		:rule (fn flat-fn [tokens]
         			(let [result (apply-parser parser tokens)]
          				(if (pass? result) (first (:data result)) result)))}))

(comment
 	"
 	If I'm not mistaken, the Ludus grammer requires *no* lookahead, the first token in an expression tells you what kind of expression it is:

 	Rather, there is one ambiguity: synthetic expressions can start with words or keywords.
 	A bare word can be assimilated to synthetic expressions. Interestingly, so can synthetic.

 	The parsing strategy is the same: consume as many things until you can't get anymore.

 	The fact that a bare keyword is evaluated like a literal doesn't matter.

 	So:
 	literal -> literal
 	keyword -> synthetic
 	word -> synthetic
 	( -> tuple
 	[ -> list
 	#{ -> dict
 	@{ -> struct
 	ns -> ns
 	let -> let
 	do -> pipeline

 	etc.

 	Because there's now NO lookahead, we can easily distinguish between orderings that don't match at all, and ones which match on the first token.

 	Because of that, we can also distinguish between no-match and errors

 	")


(declare expression)

(def literal (flat (choice :literal [:nil :true :false :number :string])))

(def separator (choice :separator [:comma :newline]))

(def nls? (quiet (zero+ :nls :newline)))

(def pattern (choice :pattern [:literal :word])) ;; stupid to start

(def iff (order :iff [
                     	(quiet :if) 
                     	nls? 
                     	expression 
                     	nls? 
                     	(quiet :then) 
                     	expression 
                     	nls? 
                     	(quiet :else) 
                     	expression]))

(def lett (order :let [
                      	(quiet :let)
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

(def splat (order :splat [(quiet :splat) :word]))

(def list-term (flat (choice :list-term [splat expression])))

(def list-entry (order :list-entry [(quiet (one+ separator)) list-term]))

(def listt (order :list
            	[(quiet :lbracket)
            		(quiet (zero+ separator))
            		(maybe list-term)
            		(zero+ list-entry)
            		(quiet (zero+ separator))
            		(quiet :rbracket)]))

(def synth-root (choice :synth-root [:keyword :word]))

(def synth-term (choice :synth-term [tuple :keyword]))

(def synthetic (order :synthetic [synth-root (zero+ synth-term)]))

(def terminator (choice :terminator [:newline :semicolon]))

(def block-line (order :block-line [(quiet terminator) expression]))

(def block (order :block [(quiet :lbrace) nls? expression (zero+ block-line) nls? (quiet :rbrace)]))

(def expression (choice :expression [lett iff synthetic block listt tuple literal]))

(def importt (order :import [(quiet :import) :string (quiet :as) :word]))

(def toplevel (flat (choice :toplevel [importt expression])))

(def script-line (order :script-line [(quiet terminator) toplevel]))

(def script (order :script [nls? toplevel (zero+ script-line) nls? (quiet :eof)]))


(def eg (:tokens (scan/scan
                  	"1
                  	2
                  	3"
                  	)))

eg

(println eg)

(def result (apply-parser script eg))

result

(println result)

(defn clean [node]
 	(if (map? node)
  		(-> node 
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