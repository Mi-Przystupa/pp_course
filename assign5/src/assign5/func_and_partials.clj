(ns assign5.func-and-partials
  (:require [anglican.runtime :refer [tanh observe* normal]])
  
  )



;Math operation definitions

(defn normpdf [y m s]
  (observe* (normal m s) y))

(defn exp 
  [x]
  (Math/exp x))

(defn cos
  [x]
  (Math/cos x))

(defn sin
  [x]
  (Math/sin x))

(defn log
  [x]
  (Math/log x))

;partial derivatives of a function 
(def partial-fns
    (reduce
      merge
      (list
        {'* [(fn [a b] b) (fn [a b] a)]}
        ; f(a,b) = a * b <-> (* a b)
        ; df/da = b
        ; df/db = a

        {'- [(fn [a b] 1) (fn [a b] -1)]}
        ; f(a,b) = a - b <-> (- a b)
        ; df/da = 1
        ; df/db = -1

        {'+ [(fn [a b] 1) (fn [a b] 1)]}
        ; f(a,b) = a + b <-> (+ a b)
        ; df/da = 1
        ; df/db = 1

        {'/ [(fn [a b] (/ 1 b)) (fn [a b] (* a (/ -1 (* b b))))]}
        ; f(a,b) = a / b <-> (/ a b)
        ; df/da = 1
        ; df/db = -1/b^2

        {'exp [(fn [a] (exp a))]}
        ; f(a) = (exp a)
        ; df/da = (exp a)

        {'relu [(fn [a] (if (> a 0) 1 0))]}
        ; f(a) = (relu a)
        ; df/da = 1 if a > 0, 0 otherwise

        {'log [(fn [a] (/ 1 a))]}; <--- base e

;        {'normpdf [...]}


        {'sin [(fn [a] (cos a))]})))


