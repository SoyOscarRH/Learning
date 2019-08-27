(defun MiTipo
  (x y)
  (reverse
    (let 
      (
        (result ()) 
        (total (length x))
      ) 
      (do 
        ( (i 0 (+ i 1)) )
        ( (<= total (length result)) result )
        (push 
          (eql (type-of (nth i x)) (type-of (nth i y)) ) 
          result)
      )
    )
  )
)

(print (MiTipo (list 'b 5 T) (list 'A 3 T) ))