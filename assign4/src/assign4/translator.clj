(ns assign4.translator
  (:use assign4.parser) 
  (:use assign4.primitives)
  (:require [anglican.runtime :refer :all])
  (:require [anglican.core :refer :all])
  (:require [clojure.core.matrix :as m])
  (:require [anglican.runtime :refer [tanh]])
  (:require [instaparse.core :as insta :refer [defparser]]) 

  )



;(println *primitive-procedures*)

(def G {:V {} :A {} :P { } :Y { }})

(def rho {}); a global variable of user defined functions....
(defn process_var
	[token]
	{:G G :E (rest token)}
)

(defn procee_constant
	[token ] 
	{:G G :E (read-string (rest token))}
)

(defn process_if 
	[token ]
	(println "implement me")
	{:G G :E (rest token)	}
)

(defn process_user_op 
	[token]
	(println "implement me")
	{:G G :E (rest token)	}
)

(defn process_prim 
	[token]
	(println "implement me")
	{:G G :E (rest token)	}
)

(defn process_sample
	(println "implement me")
	{:G G :E (rest token)	}
)

(defn process_observe
	(println "implement me")
	{:G G :E (rest token)	}
)

(defn process_expression
	[token ] 
	(cond
		(in? :v token) (process_variable token)
		(in? :if_op token) (process_if token)
		(in? :user_op token) (process_user_op token)
		(in? :let_op token) (process_let token)
		(in? :prim_op token) (process_prim token)	
		(in? :sample_op token) (process_sample token )
		(in? :observe_op token) (process_observe token )
	)

)
(defn process_let
	[token ]
	(let [ v (process_var (get token 2))
		e1 (get token 3)
		GE1 (proces_expression e1) 
		e2 (get token 4)
		
		]
	
		
	
	)
)

(defn process_token
	[token ]

	(cond

		(in? :let_op) (proces_let ( token))
		(in? :sample) (proces_sample)
	)

)



(defn translate
  [ tree] 
	
	(loop [rho {} phi [] t tree g G E nil ] 
		(if (empty? t)
			g
		(let [f (first t)
			 r (rest t)
			 GE (rule_on rho f g E) ] 
		(recur 
			r (merge_graph g (get GE :G)) (get GE :E)
			)

		)
		)
	)
)
		


