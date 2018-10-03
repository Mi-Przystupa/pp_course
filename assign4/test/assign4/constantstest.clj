(ns assign4.constantstest
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


(deftest number_constants
  (check_eq [:q [:e [:c "5"]]] (foppl "5"))
  (check_eq [:q [:e [:c "1.2345"]]] (foppl "1.2345"))
  (check_eq [:q [:e [:c "135209"]]] (foppl "135209"))
  (check_eq [:q [:e [:c "1325.0"]]] (foppl "1325.0")) 
  )



(deftest bool_constants
  (check_eq [:q [:e [:c "true"]]] (foppl "true"))
  (check_eq [:q [:e [:c "false"]]] (foppl "false"))
  )

(deftest str_constants
  (check_eq [:q [:e [:c "applesauce"]]] (foppl "\"applesauce\""))
  (check_eq [:q [:e [:c "123"]]] (foppl "\"123\""))
  (check_eq [:q [:e [:c "apple_sauce3"]]] (foppl "\"apple_sauce3\""))
  ) ;TODO not entirely sure if I trust this 


(deftest distribution_constants
  (check_eq [:q [:e [:c "normal"]]] (foppl "normal"))
  (check_eq [:q [:e [:c "uniform-continuous"]]] (foppl "uniform-continuous"))
  (check_eq [:q [:e [:c "beta"]]] (foppl "beta"))
  (check_eq [:q [:e [:c "bernoulli"]]] (foppl "bernoulli"))
  (check_eq [:q [:e [:c "discrete"]]] (foppl "discrete"))
  )

(deftest data_structures
  (check_eq [:q [:e [:c "list"]]] (foppl "list"))
  (check_eq [:q [:e [:c "hash-map"]]] (foppl "hash-map"))
  )

(deftest list_operations
  (check_eq [:q [:e [:c "first"]]] (foppl "first"))
  (check_eq [:q [:e [:c "rest"]]] (foppl "rest"))
  (check_eq [:q [:e [:c "last"]]] (foppl "last"))
  (check_eq [:q [:e [:c "nth"]]] (foppl "nth"))
  (check_eq [:q [:e [:c "conj"]]] (foppl "conj"))
  (check_eq [:q [:e [:c "cons"]]] (foppl "cons"))
  (check_eq [:q [:e [:c "append"]]] (foppl "append"))
  )

(deftest arithmetic_operations
  (check_eq [:q [:e [:c "+"]]] (foppl "+"))
  (check_eq [:q [:e [:c "-"]]] (foppl "-"))
  (check_eq [:q [:e [:c "/"]]] (foppl "/"))
  (check_eq [:q [:e [:c "="]]] (foppl "="))
  )

(deftest distr_operations
  (check_eq [:q [:e [:c "sample"]]] (foppl "sample"))
  (check_eq [:q [:e [:c "observe"]]] (foppl "observe"))
  )

(deftest matrix_operations
  (check_eq [:q [:e [:c "mat-mul"]]] (foppl "mat-mul"))
  (check_eq [:q [:e [:c "mat-repmat"]]] (foppl "mat-repmat"))
  (check_eq [:q [:e [:c "mat-add"]]] (foppl "mat-add"))
  (check_eq [:q [:e [:c "mat-tanh"]]] (foppl "mat-tanh"))
  (check_eq [:q [:e [:c "mat-transpose"]]] (foppl "mat-transpose"))
  )




