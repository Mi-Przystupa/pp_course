(ns assign5.backprop
  (:use assign5.func-and-partials)
  (:use assign5.programs)
  )

;(nth (nth (vec (list '(let [x (sample (normal 0 123))] x))) 0) 0)

(defn varName
  [number]
  (format "v_%d" number)

  )
(defn addVariables
  [vars e v w_l]
  ;-vars is map of input variables to values we are evaluating them on. 
  ; --eg. {'x 1} means there is 1 input variable x
  ;-e at this point it means it's a variable 
  ;-v index of variable name to use
  ;-w_l our wengert list so far 
  (let [v_n (varName v) 
        
        ] 
    (conj (conj w_l  {v_n e} ) {e (get vars e)} ) ; assumes w_l is a list so conj will append to beginning 
  ))


(defn one_input
  [w_l c v] 
  ;-w_l wengert list so far
  ;-c the operand done on the single input
  ;-v index of current variable
  (let [var1 (varName v) ;current variable v_n 
        var2 (varName (inc v)) ] ;since we know it's a one input function, it'll depend on the next variable
    (conj  w_l {var1 (list c var2)}) ; adds :v_n (c v_n+1)
  ))

(defn recursiveBuildWengert
  [vars body w_l v]
  ;-vars is map of input variables to values we are evaluating them on. 
  ; --eg. {'x 1} means there is 1 input variable x
  ;-body is a list with all the expresions to be evaluated
  ;-w_l is our accumulator for the wengert list so far
  ;--it is a list of maps, each map is of form {:var expression}
  ;-v is our current variable

  (let [e (if (symbol? body) body (first body)) ; if symbol just use that, else take first elemnt of sequence
        ]
    (cond
      (contains? vars e) (addVariables vars e v w_l) ; means it is a variable
      (= (count body) 2) (recursiveBuildWengert vars (last body) (one_input w_l e v) (inc v)) ;an epression of 1 input
      (= (count body) 3) 3; an expression of 2 inputs
      (= (count body) 4) 3; an expression of 3 inputs
      )


  ))

(defn wengertList
  [vars body]
  ;vars is map of input variables to values we are evaluating them on. 
  ; -eg. {'x 1} means there is 1 input variable x
  ;body is a list with all the expresions to be evaluated

  (recursiveBuildWengert vars body (list {:f (varName 0)}) 0)
    )
(defn updateValue
  [cur_var expr w_l]
  ;cur_var is the key fo the current variable
  ;expr is the value we're going to either update or just return 
  ;w_l the wengert list we're mutating
  (cond
    (list? expr) ; means it's something like: (c v_0 ... v_n)
      (let [c (resolve (first expr)) ; get the current operation
        args (map (fn [x] (get w_l x)) (rest expr)) ; get the values for arguments (should have been evaluated already 
        value (apply c (into [] args)) ; perform operation 
        updated (update w_l cur_var (fn [x] value)) ; mutate dictionary to get rid of expresion 
            ]
        updated

        ;call eval on (c args) set args to be entries from map
        )
    (or (symbol? expr) (= :f cur_var)) ; either means it's one of our inputs or we're at the end, in which case get previous value
      (let [old_w_l w_l
            new_w_l (update w_l cur_var (fn [x] (get old_w_l x))) ; change dictionary with current value
            ]
        new_w_l
        )
    :else w_l ; do nothing, likely means we just started
    )
  
  )  

(defn forward
  [w_l]
  ;-w_l use wengert list to build forward pass
  ;-- should contain maps in order of : {key expression}

  (let [w_l_map (into {} w_l) ; convert w_l to map to easily access previously evaled variables
        vars (keys w_l_map) ; all the variable names to be evaluated
        ;^ important thing: these keys need to be in order: v0,v1,v2,v3,v4 so if you get bugs this could be issue 
        ] 

   ;this...is probably going to only work for 1 input rn 
   (loop [ to_do vars ;variables to be processed
          updates w_l_map] ; dict to be mutated: (var expr) => (var value)    
     (let [f (first to_do)
           r (rest to_do) 
           next_exp (get updates f)
           updated (updateValue f next_exp updates)
           ]

       (if (empty? to_do)
          updated  ; :f should contain the output if this worked 
          (recur r updated)
         )
      )
       )))

(defn handleDelta
  [z y_i args op_i values delta]
  (let [ d (if (contains? delta y_i) delta (assoc delta y_i 0) )
        cur_der (get d y_i)
        val_args (into [] (map #(get values %) args))
        op_val (apply op_i val_args) 
        new_val (+ cur_der (* (get d z) op_val))
        ] 
    (assoc d y_i new_val)
  ))


(defn updateDelta 
  [entry values delta]
  ; entry is map of current expression to handle
  ; values is a map of each variable done in forward pass
  ; delta is current derivatives
  ;(println entry)
  ;(println values)
  ;(println delta)
  (let [ z (first (keys entry)) ; variable we are handling
        v (get entry z) ; body expression to date derivative of
        ]
    (if (seq? v)
      (let [
        g (first v) ; means first element is sequence 
        op (get partial-fns g)
        args (rest v)
        
        ] 
      (loop [to_do args
             i 0 ; 
              op_i (get op i)
             update_delta delta
             ]
        (if (empty? to_do) update_delta
          (let [y_i (first to_do) 
                r (rest to_do)]
                (recur  (rest to_do)
                          (inc i)
                          (op_i (inc i))
                          (handleDelta  z y_i args op_i values delta)
                        
          ))))) 
      (if (not (number? v))
        (let [d (if (contains? delta v) delta (assoc delta v 0))]
         (assoc d v (+ (get d v) (d z))) 
         
        )
        delta ; if it's a number then we're done 
        ))))

(defn backward
 [values  w_l]
  ;-values are the forward pass values
  ;-w_l is our list of maps corresponding to each operation done in graph
  (loop [to_do (reverse w_l) ; rest because :f v_0 is redundant
         delta {:f 1} ]
    (if (empty? to_do)
      delta
      (let [ f (first to_do) ; 
             r (rest to_do)
            ]
        (recur r (updateDelta f values delta))
      )

    )

    )
  )

(defn autograd
  [f inputs]

  ;(println function) 
  
  ;(println (list function))
  (let [ [op args body] f 
        vars (zipmap args inputs) ; creats a map between the vector args and inputs. e.g. [x] [1] => {'x 1}
        w_l (wengertList vars body) ; create the wengert list
        values  (forward w_l ) ; do the forward pass 
        derivs (backward values w_l) ;calculate the backwad pass
        ]
   (println w_l)
   ;(println values)
   (println "done reverse dif")
   (println derivs) 
   (list (get values :f) (zipmap (map #(format "df/d%s" %)args) (map #(get derivs %) args ))) ;this gives the value
      ))


(autograd prog1 [3])
(autograd prog4 [3])
;(or (instance? Long f) (symbol? f ) (empty? f) (number? f) )

 
