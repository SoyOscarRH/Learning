;2x^2+7x+5=0

(setq a 3)
(setq b 5)
(setq c 2)

(print 
    (list 
        (/ 
            (+ 
                (- b) 
                (sqrt 
                    (-  
                        (* b b) 
                        (* 4 a c) ) )  )
            (* 2 a))

        (/ 
            (- 
                (- b) 
                (sqrt 
                    (-  
                        (* b b) 
                        (* 4 a c) ) )  )
            (* 2 a)))
)