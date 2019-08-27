(defun analiza(X)
  (list 
    (atom X)
    (numberp X)
    (listp X)
    (consp X)
    (null X)
  )
)

(print (analiza NIL))