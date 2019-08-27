(defun recombina 
  (data)

  (let 
    ( 
      (arg1 (first data))
      (arg2 (second data))
      (arg3 (third data))
    )
    (let 
      (
        (A (first arg1))
        (B (first arg2))
        (C (first arg3))

        (x (rest arg1))
        (y (rest arg2))
        (z (rest arg3))
      )

      (let 
        (
          (res1 
            (cons (list x y) A))
          (res2 
            (cons (list y z) C))
          (res3 
            (cons (list z y x) B))
        )
        (list res1 res2 res3)
      )
    )
  )
)

(print (recombina '((A . x) (B . y) (C . z)) ))