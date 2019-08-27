(defun intercala
  (x y)
  (reverse
    (let 
      (
        (result ()) 
        (total (+ (length x) (length y) ) )
      ) 
      (do 
        ( 
          (i 0 (+ i 1)) 
          (j 0 (+ j 1))
        )
        ( (<= total (length result)) result )
        (if (eql (nth i x) nil) nil (push (nth i x) result) )
        (if (eql (nth j y) nil) nil (push (nth j y) result) )
      )
    )
  )
)

(print (intercala  '(X Y) '(A B C D)  ))