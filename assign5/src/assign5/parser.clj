(ns assign5.parser
  (:use assign5.func-and-partials)
  (:require [instaparse.core :as insta :refer [defparser]])
  
  )

(defparser backprop
    "fn = p 'fn' s v s <'['> s (v s)* <']'> s (e s)* cp 
      v ::=  #'[a-zA-Z][a-zA-Z0-9_/\\-?]*';
      <s> ::= <#'\\s*'>
      <e> ::= v | c | if | expr 
      <p> ::= <'('>
      <cp> ::= <')'>
      if ::= p s 'if' s e s e s e s cp  (* (if e_1 e_2 e_3) *)
      expr ::= p s c s (e s)* cp (* (c e_1 ... e_n) *)
      <c> ::= exp | sin | num 
      exp ::= <'exp'>
      sin ::= <'sin'>
      num ::= #'[0-9]+'

  
  "
  )


(defn get_partials
  [tree]

  (insta/transform
    {:sin (fn [& x] (get partial-fns 'sin))
     :v (fn [ & x] x)

     }
    tree
  ))
