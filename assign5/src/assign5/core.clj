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


(println (count (finite-difference-grad  prog6  )))
(apply (eval prog1) [1])

(def xmusi [10 0 2])
(def p7i [2 7.01 5])
(def inputs   (list [0]  [0 10] [5.000001]  [0.1] xmusi xmusi p7i  ))
(def programs (list prog1 prog2 prog3       prog4 prog5 prog6 prog7 ))
(def to_check (map vector inputs programs))


(defn check_all
  []
(loop [todo to_check ]
    (if (empty? todo)
      (println "we done")

       (let [ arg_func (first todo)
             r (rest todo)
             arg (first arg_func)
             func (last arg_func)
             [f vars body] func

             forward (apply (eval func) arg)
             aprox_deriv (finite-difference-grad func) 

             aprox_deriv (apply (eval aprox_deriv) arg);(apply (eval x) arg)) aprox_deriv)
             names  (map #(format "df/d%s" %) vars)
             forward_backward (list forward  (zipmap names aprox_deriv) )
            ] 
         (println "Program:")
         (println func)
         ;(println forward_backward)
         (autograd func arg)
         (recur r)
      )
  ;(println (autograd prog6 [0.5 0 1]))
  ;(println (apply (eval (finite-difference-grad  prog6  )) [0.5 0 1]))

  )))

(check_all)
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ; an example use of finite difference 
  ;(apply (eval (finite-difference-grad '(fn [x y] (+ (* x x) (sin x)))
  ; )) [3 5])

  
  )
;something to eval all the programs
