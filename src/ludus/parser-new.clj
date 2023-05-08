(ns ludus.parser-new
 	(:require
  		[ludus.scanner :as scan]))

(def msgs {

          	})

(defn ? [val default] (if (nil? val) default val))

(defn ok? [{status :status}]
 	(= status :ok))

(defn pass? [{status :status}] (or (= status :ok) (= status :quiet)))

(defn data [{d :data}] d)

(defn remaining [{r :remaining}] r)

(defn pname [parser] (? (:name parser) parser))

(defn value [token]
 	(if (= :none (:literal token)) (:lexeme token) (:literal token)))

(defn apply-kw-parser [kw tokens]
 	(let [token (first tokens)]
 		(println "applying kw parser " kw " to " token)
  		(if (= kw (:type token))
  			{:status :ok :type kw :data [(value token)] :token token :remaining (rest tokens)}
   			{:status :err :token token :trace [kw] :remaining (rest tokens)})))

(defn apply-fn-parser [parser tokens]
 	(println "applying fn parser" parser ", " tokens)
 	(let [rule (:rule parser) name (:name parser)] 	
  		(println "appying fn parser " name " to " (first tokens)) 
  		(rule tokens)))

(defn apply-parser [parser tokens]
 	(if (keyword? parser) 
  		(apply-kw-parser parser tokens) 
  		(apply-fn-parser parser tokens)))

(defn pmap [f parser] (fn [tokens] (f (apply-parser parser tokens))))

(defn choice [name parsers]
 	{:name name
 		:rule (fn [tokens]
 				(println "entering CHOICE" name)
         		(loop [ps parsers]
          			(let [result (apply-parser (first ps) tokens) 
               				rem-ts (remaining result)
               				rem-ps (rest ps)]
         						(cond 
          							(pass? result)
          							{:status :ok :type name :data [result] :token (first tokens) :remaining rem-ts}

          							(empty? rem-ps)
          							{:status :err :token (first tokens) :trace [name] :remaining rem-ts}
          							:else (recur rem-ps)))))})

(defn order [name parsers]
 	{:name name
 		:rule (fn [tokens] 
 					(println "entering ORDER" name)
        			(let [origin (first tokens)]
          			(loop [ps parsers 
                				results [] 
                				ts tokens]
            			(let [result (apply-parser (first ps) ts)
                 				res-rem (remaining result)]
             				(if (empty? (rest ps)) 
         						(case (:status result)
          							
          							:ok {:status :ok 
          								:type name 
          								:data (conj results result) 
          								:token origin 
          								:remaining res-rem}
          							
          							:quiet {:status :ok 
          								:type name 
          								:data results 
          								:token origin 
          								:remaining res-rem}

          							:group {:status :ok
          								:type name
          								:data (concat results (:data result))
          								:token origin 
          								:remaining res-rem}
          							
          							:err (update result :trace #(conj % name)))
             							
              					(case (:status result)  
                					:ok 	(recur (rest ps) (conj results result) res-rem)
                					:group	(recur (rest ps) 
                						;; TODO: fix this?
                						;; This is supposed to undo the :quiet/:group thing
                						(concat results 
                							(filter #(= (:status %) :ok) (:data result))) 
                						res-rem)
                					:quiet 	(recur (rest ps) results res-rem)
                					:err 	(update result :trace #(conj % name))))))))})

(defn quiet [parser]
 	{:name (? (:name parser) parser)
 		:rule (fn [tokens]
        			(let [result (apply-parser parser tokens)]
         				(if (pass? result) 
 							(assoc result :status :quiet)
         					result)))})

(defn one+
	([parser] (one+ (pname parser) parser))
	([name parser]
 		{:name name
 		:rule (fn [tokens]
        			(let [result (apply-parser parser tokens)
        				rest (zero+ name parser)]
        				(case (:status result)
        					(:ok :quiet)
        					(let [rest-result (apply-parser rest (remaining result))
        						rest-data (data rest-result)
        						rest-remaining (remaining rest-result)]
        						(println rest-data)
        						{:status :group
        							:type name 
        							:data (concat (data result) (second rest-data)) 
        							:token (first tokens)
        							:remaining rest-remaining})
        					
        					:err result)))}))

(defn zero+ 
	([parser] (zero+ (pname parser) parser))
	([name parser]
 	{:name name
 		:rule (fn [tokens]
 					(println "entering ZERO+")
        			(loop [results [] 
             					ts tokens
             					back tokens]
             			(println "looping ZERO+" (:name parser))
         				(let [result (apply-parser parser ts)]
          					(if (pass? result)
           						(recur (conj results result) (remaining result) ts)
           						{:status :group :type name :data results :token (first tokens) :remaining ts}
           						))))}))

(defn maybe
	([parser] (maybe (pname parser) parser))
	([name parser]
		{:name name
			:rule (fn [tokens]
				(let [result (apply-parser parser tokens)]
					(if (pass? result)
						result
						{:status :group :type name :data [] :token (first tokens) :remaining tokens}
						)))}))

(comment
	"So one thing I'm thinking about is the fact that zero+, one+, maybe all only really make sense in the context of an `order` call. So that idea is that anything that's in one of these should be added to the `order`'s data vector, rather than putting it in a subordinate structure.

	This is much the same as the `quiet` idea: there should be some kind of internal representation of the thing.

	***

	And now the `group` status has broken `quiet`


")

(defn group 
	([parser] (pname parser) parser)
	([name parser] (fn [tokens]
		(let [result (apply-parser parser tokens)
			data (map :data (:data result))]
			{assoc result :data data}))))


(declare expression)

(def literal (choice :literal [:nil :true :false :number :string :keyword]))

(def separator (one+ (choice :separator [:comma :newline])))

(def nls? (quiet (zero+ :nls :newline)))

(def tuple-entries (order :tuple-entries [(quiet separator) expression]))

(def tuple (order :tuple 
	[(quiet :lparen) 
		(maybe expression)
		(zero+ tuple-entries)
		(quiet :rparen)]))

(def expression (choice :expression [tuple literal]))

(def foo (order :foo [:number :keyword]))

(def eg (:tokens (scan/scan "(1, 2, 3)")))

(def result (apply-parser tuple eg))

result

(defn clean [node]
	(if (map? node)
		(-> node 
			(dissoc 
				:status 
				:remaining 
				:token)
			(update :data #(map clean %)))
		node))

(defn tap [x] (println "\n\n\n\n******NEW PARSE\n\n:::=> " x "\n\n") x)

(def my-data (-> result clean tap))

my-data

(def my-first (-> my-data first))

(def my-sec (map :data (-> my-data second :data)))

(concat my-first my-sec)