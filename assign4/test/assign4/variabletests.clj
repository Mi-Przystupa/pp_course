(ns assign4.variabletests
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



(deftest variableNames
  (check_eq [:q [:e [:v "_"]]] (foppl "_"))
  (check_eq [:q [:e [:v "x"]]] (foppl "x"))
  (check_eq [:q [:e [:v "X"]]] (foppl "X"))
  (check_eq [:q [:e [:v "W_0"]]] (foppl "W_0"))
  (check_eq [:q [:e [:v "trans-dist"]]] (foppl "trans-dist"))
  )





