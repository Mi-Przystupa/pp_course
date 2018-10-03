(ns assign4.core-test
  (:require [clojure.test :refer :all]
            [assign4.core :refer :all]
            [assign4.parser :refer :all]
            [assign4.constants :refer :all]
            [instaparse.core :as insta :refer [defparser]]
))

; set up test
(defn successful_parse? [input]
  (not (insta/failure? (foppl input)))
  )

(defn test_parsing [msg input]
	(testing (str msg input)
		(is (successful_parse? input))))


; Beginning actual tests

(deftest test_parser_eval_programs
	(test_parsing message eval1)
	(test_parsing message eval2)
	(test_parsing message eval3)
	(test_parsing message eval4)
)

(deftest test_parser_other_programs
  (testing (str message prog1) 
    (is (successful_parse? "(let [x (sample (normal 0))] x)")))
  (testing (str message prog2)
    (is (successful_parse? prog2))) 
  (testing (str message prog3)
	(is (successful_parse? prog3)))

)
