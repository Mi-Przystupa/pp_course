(ns assign4.translationtests
      (:require [clojure.test :refer :all]
            [assign4.translator :refer :all]
            [assign4.parser :refer :all]
            [assign4.constants :refer :all]
            [instaparse.core :as insta :refer [defparser]]
))


(defn check_map
  [ msg G ]
  (testing msg
    (is (contains? G :V))
    (is (contains? G :P))
    (is (contains? G :A))
    (is (contains? G :Y) )
  )
)  

(defn check_empty
  [msg G]
  (testing msg
    (is (empty? (get map :V)))
    (is (empty? (get map :P)))
    (is (empty? (get map :A)))
    (is (empty? (get map :Y)))

    )
)

(deftest test_constants_and_vars
  
  (let [ G_1 (translate (foppl "+") G nil)
        G_2 (translate (foppl "var") G nil) 
        G_3 (translate (foppl "15.0") G nil) ]
    (check_map "primitve operand" G_1)
    (check_map "variable map" G_2)
    (check_map "number primitive" G_3)

    (check_empty "constants have no graph" G_1)
    (check_empty "variables produce no graph" G_2)
    (check_empty "numbers produce no graph" G_3)  

  )
)


(deftest test_let_translations
  (let [G_1 (translate (foppl "(let [z 7] z)"))
        G_2 (translate (foppl "(let [alpha (+ 3 4)] (/ 7 4))"))
                              ]

    (check_map "example from class" G_1)
    (check_map "a slightly more complex example" G_2)
  )
)



