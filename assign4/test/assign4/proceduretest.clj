(ns assign4.proceduretest
    (:require [clojure.test :refer :all]
            [assign4.core :refer :all]
            [assign4.parser :refer :all]
            [assign4.constants :refer :all]
            [instaparse.core :as insta :refer [defparser]]
))

(defn check_eq
  [expected actual]
  (testing actual
    (is (=  expected actual )))
)

; set up test
(defn successful_parse? [input]
  (let [result (foppl input) ] 
    (not (insta/failure? result ))
  )
)

(defn test_parsing [msg input]
	(testing (str msg input)
		(is (successful_parse? input))))




(deftest create_procedures_followed_by_prims 
  (test_parsing "user defined function followed by number " "(defn sum [a b] (+ a b)) 10.0") 
  (test_parsing "user def followed by var" "( defn sum [ a b c d] (+ a b c)) variable")
  (test_parsing "user def followed by operation" "(defn sum [app _ ] (+ app _)) +") 
  (test_parsing "user def folowed by mat_op" "(defn sum [ ] (+ 10.0 5.0)) mat-mul")
  )


(deftest create_procedure_call_procedure
  (test_parsing "user defn func then call it" "(defn sum [a b] (+ a b)) (sum 5.0 10.0)")

  (test_parsing "user defn func with nested expression" "(defn sum_sub [a b c] ( - (+ a b) c)) (sum_sub 10.0 3.0 7)")

  )


(deftest create_several_procedures_then_primitive
  (test_parsing "defn 2 functions" 
    "(defn fn_a [a b] (mat-mul a b)) (defn fn_b [u o] (normal u o)) (fn_a (sample (fn_b 0.0 1.0)) 4)")

  (test_parsing "defin 3 functions"
    "(defn fn_a [a] (mat-tanh a))
     (defn fn_b [b] (bernoulli b))
     (defn fn_c [u o] (sample (normal u o)))
     (fn_c (fn_a 5.0) (sample (fn_b 5)))"
  )
)





