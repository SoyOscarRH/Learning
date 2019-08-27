(defun Bisiesto (year)
  (or
   (and 
      (zerop (mod year 4))
      (not (zerop (mod year 100)))
    )
    (zerop (mod year 400))
  )
)

(print (Bisiesto 2020))