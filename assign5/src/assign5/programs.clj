(ns assign5.programs)

;1)
;((resolve (symbol (str 'prog1)))  4) 

(def prog1 '(fn [x] (exp (sin x))) )

;2)
(def prog2 '(fn [x y] (+ (* x x) (sin x))))
;3)
(def prog3 '(fn [x] (if (> x 5) (* x x) (+ x 18))))
;4)
(def prog4 '(fn [x] (log x)))
;5)
(def prog5 '(fn [x mu sigma] (+ (- 0 (/ (* (- x mu) (- x mu))
                                                  (* 2 (* sigma sigma))))
                                       (* (- 0 (/ 1 2)) (log (* 2 (* 3.141592653589793 (* sigma sigma)))))))
  )
;6)
(def prog6 '(fn [x mu sigma] (normpdf x mu sigma)))
;7)
(def prog7 '(fn [x1 x2 x3] (+ (+ (normpdf x1 2 5)
                        (if (> x2 7)
                          (normpdf x2 0 1)
                          (normpdf x2 10 1)))
                    (normpdf x3 -4 10))))


