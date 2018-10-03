(ns assign4.parser
  (:require [instaparse.core :as insta :refer [defparser]]) 

  )

; will need to add strings at some point :/


(defparser parse_me 
  " 
    q ::= e| define <w_s> q <w_s>  ; 
  
    (*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)  

    <e>::= underscore | v | c | sugar |user_op |  let_op | if_op | prim_op | sample_op | observe_op  ;
    prim_op ::= <'('> <o_s> c <w_s>  e (<w_s> e )* <o_s>  <')'> 
    user_op ::= <'('> <o_s> f <w_s>  e (<w_s> e )* <o_s>  <')'> 
    sample_op ::= <'('> <o_s> 'sample' <w_s> e  <o_s>  <')'> 
    observe_op ::= <'('> <o_s> 'observe' <w_s>  e <w_s> e <o_s>  <')'> 
    if_op ::= <'('> <o_s> 'if' <w_s>  e <w_s> e <w_s> e <o_s>  <')'> 
    let_op ::= <'('> <o_s> 'let' <w_s> <'['> v <w_s> e <o_s> <']'> <w_s> e <o_s> <')'> 

	ReservedWords := 'if' | 'let' | 'foreach' | 'loop' | operands | distr_op

    (*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
    <sugar> ::= vector_sugar | map_sugar | let_sugar | loop | foreach ;
    vector_sugar ::= <'['> <o_s> (e <w_s>)* e* <o_s> <']'>
    map_sugar ::= <'{'> <o_s> (e <w_s> e <w_s>)* (e <w_s> e)* <o_s> <'}'>
    let_sugar ::= <'('> <o_s> 'let' <w_s> <'['> (v <w_s> e ) <w_s> (v <w_s> e <o_s>)+ <']'> (<w_s> e)+ <')'> 
    loop ::= <'('> <o_s> 'loop' <w_s> integer <w_s> e <w_s> f <w_s> e (<w_s> e)* <o_s> <')'>
    foreach ::= <'('> 'foreach' <w_s> integer <w_s> <'['> <o_s> ( v <w_s> e)* <o_s> <']'> <w_s> e (<w_s> e)* <o_s> <')'> 
 
    (*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)
    c ::= number | bool | str | data_struct | operands;
    <number> = integer | real ;
    <integer> = #'-?[0-9]+' ;
    <real> = #'-?[0-9.]+' ;
    <w_s> = #'\\s*'; (* mandatory white space *)
    <o_s> = #'\\s*'; (*optional white space *)
    <bool> = 'true' | 'false';
    <str> = <'\"'> #'\\w+' <'\"'>; 
    <data_struct> = 'list' | 'hash-map' | 'vector';
    <operands> = arthimetic | dist_op |  list_op | distr_op | mat_op  ;
    <arthimetic> = '+' | '-' | '/' | '=' | '*' | 'sqrt';
    <dist_op> = 'normal' | 'uniform-continuous' | 'beta' | 'bernoulli' | 'discrete';
    <list_op> = 'first' | 'rest' | 'last' | 'nth' | 'conj' | 'cons' | 'append';
    <distr_op> = 'sample' | 'observe';
    <mat_op> = 'mat-mul' | 'mat-repmat' | 'mat-add' | 'mat-tanh' | 'mat-transpose';

    (*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*) 
    v ::=  #'[a-zA-Z][a-zA-Z0-9_/\\-?]*';
	underscore ::= '_'

    (*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*)

    f ::=  #'[a-zA-Z][a-zA-Z0-9_/-?]*' !ReservedWords  ;
    define ::= <'('> <o_s> 'defn' <w_s>  f <w_s> '[' <o_s> v* (<w_s> v)*   <o_s> ']' <w_s> e <o_s> <')'> ; 
   


  "
)

(defn in?
    [elem coll]
    (some #(= elem %) coll))


(defn do_desugar_let
	[l args]
  (if (< (count args) 2)
	
	[args]
  (let [f (first args)
        n (first (rest args))
		r (rest (rest args))
		]
		
    [:let_op l
		(if (in? :v f)
			f [:v "_"] 	
		)
		(if (in? :v f)
			n f
		)
		(if (in? :v f)
			(do_desugar_let l r )
			(do_desugar_let l (rest args))
		)
	]
  )

)
)


(defn desugar_let
  [l & args]
	(do_desugar_let l args)
)
(defn desugar
  [tree]
  (insta/transform
   {:vector_sugar  (fn [ & x] [:c "vector"  x ] ) 
    :map_sugar (fn [& x] [:c "map" x])
    :let_sugar desugar_let
    :underscore (fn [x] [:v (gensym) ])
;	:loop desugar_loop
     }
    tree)
  )

(defn foppl
  [program]
  (desugar
  (parse_me program))
  )

