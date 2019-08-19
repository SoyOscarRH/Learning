(defun main 
    ()
    (with-open-file 
        (stream "./input")
        (print 
            (read stream)))
)
