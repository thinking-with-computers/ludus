(ns ludus.parser)

(defn ? [val default] (if (nil? val) default val))

(defn ok? [{status :status}]
 	(= status :ok))

(def failing #{:err :none})

(def passing #{:ok :group :quiet})

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
  		;(if (= kw (:type token)) (println "Matched " kw))
  		(if (= kw (:type token))
   			{:status :ok 
   				:type kw 
   				:data (if (some? (value token)) [(value token)] [])
   				:token token 
   				:remaining (rest tokens)}
   			{:status :none :token token :trace [kw] :remaining (rest tokens)})))

(defn apply-fn-parser [parser tokens]
 	(let [rule (:rule parser) name (:name parser) result (rule tokens)]
  		;(if (pass? result) (println "Matched " (:name parser)))
  		result))

(defn apply-parser [parser tokens]
 	;(println "Applying parser " (? (:name parser) parser))
 	(let [result (cond 
               		(keyword? parser) (apply-kw-parser parser tokens)
               		(:rule parser) (apply-fn-parser parser tokens)
               		(fn? parser) (apply-fn-parser (parser) tokens)
               		:else (throw (ex-info "`apply-parser` requires a parser" {})))]
  		;(println "Parser result " (? (:name parser) parser) (:status result))
  		result
   	))

(defn choice [name parsers]
 	{:name name
 		:rule (fn choice-fn [tokens]
         		(loop [ps parsers]
          			(let [result (apply-parser (first ps) tokens) 
               				rem-ts (remaining result)
               				rem-ps (rest ps)]
         						(cond
          							(pass? result)
          							{:status :ok :type name :data [result] :token (first tokens) :remaining rem-ts}

          							(= :err (:status result))
          							(update result :trace #(conj % name))

          							(empty? rem-ps)
          							{:status :none :token (first tokens) :trace [name] :remaining rem-ts}

          							:else (recur rem-ps)))))})

(defn order-1 [name parsers]
 	{:name name
 		:rule (fn order-fn [tokens]
        			(let [origin (first tokens)
             				first-result (apply-parser (first parsers) tokens)]
          			(case (:status first-result)
           				(:err :none)
           				(assoc (update first-result :trace #(conj % name)) :status :none)

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

(defn order-0 [name parsers]
 	{:name name
 		:rule (fn order-fn [tokens]
        			(let [origin (first tokens)]
          			(loop [ps parsers 
                				results [] 
                				ts tokens]
            			(let [result (apply-parser (first ps) ts)
                 				res-rem (remaining result)]
             				(if (empty? (rest ps))
             						;; Nothing more: return 
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
             						
             						;; Still parsers left in the vector: recur
              					(case (:status result)  
                					:ok 	(recur (rest ps) (conj results result) res-rem)
                					:group	(recur (rest ps) 
                        						(vec (concat results (:data result)))
                        						res-rem)
                					:quiet 	(recur (rest ps) results res-rem)

                					(:err :none)
                					(assoc (update result :trace #(conj % name)) :status :err)

                					(throw (ex-info (str "Got bad result: " (:status result)) result))))))))})

(defn weak-order [name parsers]
 	{:name name
 		:rule (fn order-fn [tokens]
        			(let [origin (first tokens)]
          			(loop [ps parsers 
                				results [] 
                				ts tokens]
            			(let [result (apply-parser (first ps) ts)
                 				res-rem (remaining result)]
             				(if (empty? (rest ps))
             						;; Nothing more: return 
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
              							(update result :trace #(conj % name)))
             						
             						;; Still parsers left in the vector: recur
              					(case (:status result)  
                					:ok 	(recur (rest ps) (conj results result) res-rem)
                					:group	(recur (rest ps) 
                        						(vec (concat results (:data result)))
                        						res-rem)
                					:quiet 	(recur (rest ps) results res-rem)

                					(:err :none)
                					(update result :trace #(conj % name))))))))})


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
         			(loop [results [] 
              					ts tokens]
          				(let [result (apply-parser parser ts)]
           					(case (:status result)
            						:ok (recur (conj results result) (remaining result))
            						:group (recur (vec (concat results (:data result))) (remaining result))
            						:quiet (recur results (remaining result))
            						:err (update result :trace #(conj % name))
            						:none {:status :group 
                  							:type name 
                  							:data results 
                  							:token (first tokens) 
                  							:remaining ts}))))}))

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
            						(case (:status rest-result)

             							(:ok :group :quiet) 
             							{:status :group
 	            							:type name 
 	            							:data (vec (concat (:data first-result) (data rest-result)))
 	            							:token (first tokens)
 	            							:remaining (remaining rest-result)}

             							:none {:status :group :type name
                   								:data first-result
                   								:token (first tokens)
                   								:remaining (remaining rest-result)}

             							:err (update rest-result :trace #(conj % name))))
    						
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

(defn group
 	([parser] (group (pname parser) parser))
 	([name parser]
 		{:name (kw+str name "-group")
  		:rule (fn group-fn [tokens]
         			(let [result (apply-parser parser tokens)]
          				(if (= :group (:status result))
           					(assoc result :status :ok)
           					result)))}))

(defn err-msg [{token :token trace :trace}]
 	(println "Unexpected token " (:type token) " on line " (:line token))
 	(println "Expected token " (first trace)))

(defmacro defp [name & items]
 	(let [arg (last items)
     			fns (into [] (butlast items))]
  		`(defn ~name [] ((apply comp ~fns) (keyword '~name) ~arg))))
