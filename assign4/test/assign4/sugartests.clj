(ns assign4.sugartests
    (:require [clojure.test :refer :all]
            [assign4.core :refer :all]
            [assign4.parser :refer :all]
            [assign4.constants :refer :all]
            [instaparse.core :as insta :refer [defparser]]
))

(defn check_eq
  [expected actual]
  (testing actual
    (is (=  expected (foppl actual) )))
)

(defn q_e_tags ; for those expressions where we expect program then expression plus all the stuff inside
  [tags]
  [:q [:e [tags]]]
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





(deftest vector_tests
  (test_parsing "just a vector" "[ 5.0 a +]")
  (test_parsing "numbers "  "[1.2345 -13.0 6.0]")
  (test_parsing "nested vector"  "[ [ a 10.0] -13.0]") 
  (test_parsing "empty vector"  "[]") 
  (test_parsing "in an expression"  "(+ [1 2 3] 2)")
)


(deftest map_tests ;TODO maps aren't in any of the programs, so I'm not going to worry about :keyword elements atm
  (test_parsing "empty map"  "{     }")
  (test_parsing "single entry"  "{ + 10}")
  (test_parsing "single entry"  "{ 24 24}")
  (test_parsing "multi entry"  "{ \"key\" 26  \"another\" (* 7 6)}") 
  (test_parsing "in expression"  "(+ { + 10} {\"key\" apple * 6})") 
  )

(deftest let_tests

  (test_parsing "2 vars & expressions" "(let [v_1 (+ 4 5) v_2 (+ 3 -2)] (* v_1 6.0) (* v_2 2.0))")
  (test_parsing "3 vars & expressions" "(let [v_1 (+ -5 3) v_2 7 v_3 /] (v_3 5 2) (+ v_1 v_2) (- v_2 v_1))")
  (test_parsing "2 vars & 3 expressions" "(let [v_1 (+ -5 3) v_2 7 ] (* 5 2) (+ v_1 v_2) (- v_2 v_1))")
  (test_parsing "3 vars & 2 expressions" "(let [v_1 (+ -5 3) v_2 7 v_3 /] (v_3 5 2) (+ v_1 v_2))")
  )
(deftest loop_tests
  (test_parsing "a loop" "(loop 5 + 4 5)") 
  )

(deftest foreach_tests
  (test_parsing "a foreach loop" "(foreach 5 [x [1 2 3 4 5] y [ 3 2 4 5 1]] (+ x y))" )

  )
