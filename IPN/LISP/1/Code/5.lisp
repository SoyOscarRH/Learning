(defun realNoCero(N)
  (and 
    (not (eql N 0)) 
    (realp N)
  )
)

(print (realNoCero 'a))