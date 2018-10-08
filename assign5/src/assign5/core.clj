(ns assign5.core
  (:use assign5.func-and-partials)  
  ;(:use assign5.parser)

  (:use assign5.programs)
  (:use assign5.backprop)
  (:require [instaparse.core :as insta :refer [defparser]]) 
  (:require [anglican.runtime :refer [tanh observe* normal]])

  )



; finite difference code for checking an expression:

(defn addd [exprl i d]
  (if (= i 0)
    (reduce conj [`(~'+ ~d ~(first exprl))] (subvec exprl 1))
    (reduce conj (subvec exprl 0 i)
            (reduce conj [`(~'+ ~d ~(get exprl i))] (subvec exprl (+ i 1))))))

(defn finite-difference-expr [expr args i d]
  `(~'/ (~'- (~expr ~@(addd args i d)) (~expr ~@args)) ~d))

(defn finite-difference-grad [expr]
  (let [[op args body] expr
        d (gensym)
        fdes (map #(finite-difference-expr expr args % d) (range (count args)))
        argsyms (map (fn [x] `(~'quote ~x)) args)]
    `(~'fn [~@args]
       (~'let [~d 0.001]
         ~(zipmap argsyms fdes)))))

; Programs

(def f (fn [a b c]
    (/ (+ a (* 7 b)) (sin c))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ; an example use of finite difference 
  ;(apply (eval (finite-difference-grad '(fn [x y] (+ (* x x) (sin x)))
  ; )) [3 5])

  (println (autograd prog2 [3 4]))

  )

;something to eval all the programs
(comment
(loop [ x (vector prog1 prog2 prog3 prog4 prog5 prog6 prog7)]
    (println "new program")
    (println (first x))
    (println (autograd (first x)))

    (if (empty? x)
      (println "all done")
      (recur (rest x))
          )

  )
  )
