(defun APalíndromo (str)
  (concatenate 'string str (reverse str))
)

(print (APalíndromo "Hola"))