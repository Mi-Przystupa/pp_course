(ns assign4.core
  (:use assign4.parser clojure.data)
  (:use assign4.constants)
  (:use assign4.translator)
  (:require [instaparse.core :as insta :refer [defparser]]) 
  )


(defn -main
  "I don't do a whole lot ... yet."
  [& args]

	;(insta/visualize (foppl "(let [v 10 v1 5] (+ v v1))"))
	(println (foppl "(let [v 10] 10)"))	
;	(insta/visualize (foppl "(let [v 10] 10)"))
;  (insta/visualize (foppl "(if (= 10 10) true false)"))
  ;(println (translate (foppl "+") G nil))

  ;(println (translate (foppl "(let [v 10] 10)") G nil))	
  ;(*
  ;(println (foppl prog1))
  ;(println (foppl prog2))
  ;(println (foppl prog3))

  (println "eval 1")
  (println (foppl eval1))
  (insta/visualize (foppl eval1))
  ;(println "eval 2")
  ;(println (foppl eval2))
  ;(println "eval 3")
  ;(println (foppl eval3))
  ;(println "eval 4")
  ;(insta/visualize (foppl eval4))
  ;(println (foppl eval4))

  ;)
)
