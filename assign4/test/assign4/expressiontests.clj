(ns assign4.expressiontests
    (:require [clojure.test :refer :all]
            [assign4.core :refer :all]
            [assign4.parser :refer :all]
            [assign4.constants :refer :all]
            [instaparse.core :as insta :refer [defparser]]
))





; set up test
(defn successful_parse? [input]
  (let [result (foppl input) ] 
    (not (insta/failure? result ))
  )
)

(defn test_parsing [msg input]
	(testing (str msg input)
		(is (successful_parse? input))))



(let [b [:q [:c 10]]]
  (in? :c b)
  )


(deftest check_prim 
  (test_parsing "primitive addition" "(+ 10.0 15.0)") 
  (test_parsing "subtraction with multiple outputs"   "(- 10 14.0 1.2345)") 
  (test_parsing "valid in grammar but functionality would fail"  "(mat-mul 24 346)") 
  )
(deftest check_user_procedure
  (test_parsing "custom operation"  "(my_operatio 14.0 \"applesauce\" )")
  (test_parsing "Using a string"  "(This_isoperation \"ap235\")")
  (test_parsing "custom function for arbitrary inputs" "(A_function 1 2 3 4 5 6 7 8 9 abc a_10)")
)


(deftest check_sample
  (test_parsing "try a normal"  "(sample (normal 10.0 5))")
  (test_parsing  "try a beta" "( sample (beta 10))")
)

(deftest check_observe
  (test_parsing "observe a normal" "( observe (normal 0.0 1.0) 10)")
  (test_parsing "observe a discrete" "(observe (discrete (vector 0.5 0.5)) 0)")
  )

(deftest check_if
  (test_parsing "check a statement" "(if true 10 (normal 0.0 5))")
  (test_parsing "an if with 3 expression" "( if (= 10.0 x) (bernoulli 10) (+ 6 10))") 
  (test_parsing "check with procedure" "(if (val? tree) true false)")
  )


