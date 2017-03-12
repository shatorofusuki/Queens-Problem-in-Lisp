;print or ommit the output
(defparameter printresults 1)

(defparameter curr 0 )
(defparameter currlist nil)
(defparameter i 0)
(defparameter arr nil)
(defparameter limit 4)
(defparameter totalfound 0)

;main function
;optional parameter X is the size of the field. default is 4x4
(defun start (&optional x)
    (setf limit 4)
    (setf totalfound 0)
    (setf i 0)
    (cond ((not(null x))
           (setf limit x))
    )
    (inception)
    (format t "Total number : ~s~%" totalfound)
)

(defun inception (&optional i arr)
    ;manage initial call
    (cond ((null i) (setf i 0) (setf arr nil)))

    (cond
        ((= i limit)
        (cond ((AND (checklistV arr) (AND (checklistD1 arr) (checklistD2 arr)))
            (if (= printresults 1)
                (printlist arr)
            )
            (setf totalfound(+ totalfound 1))
            )
        )
        )
        ;another condition
        ((not(= i limit)) ;otherwise backtrack
            (cond ((AND (checklistV arr) (AND (checklistD1 arr) (checklistD2 arr)))
                (dotimes (a limit)
                    (setq curr a)
                    (setq currlist (cons curr nil))
                    (inception (+ i 1) (append arr currlist))
                )
                )
            )
        )
    )
)

(defun checklistD1 (list) ;top left -> bottom right
    (setf correct 1)
    (setf hasharray (make-array (* 2 LIMIT)))
    (setf gt 0)
    (loop for cons on list do
        (setq m (first cons))
        (if (not(null (aref hasharray (+ LIMIT (- m gt)))))
            (if (not(eq 0 (aref hasharray (+ LIMIT (- m gt)))))
                (setf correct 0)
            )
        )
        (setf (aref hasharray (+ LIMIT (- m gt))) 1)
        (if (= correct 0)
            (return)
        )
        (setf gt (+ gt 1))
    )
    (if (= correct 1) t nil)
)

(defun checklistD2 (list) ;top right -> bottom left
    (setf correct 1)
    (setf hasharray (make-array (* 2 LIMIT)))
    (setf gt 0)
    (loop for cons on list do
        (setq m (first cons))
        (if (not(null (aref hasharray (+ m gt))))
            (if (not(eq 0 (aref hasharray (+ m gt))))
                (setf correct 0)
            )
        )
        (setf (aref hasharray (+ m gt)) 1)
        (if (= correct 0) (return))
        (setf gt (+ gt 1))
    )
    (if (= correct 1) t nil)
)

(defun checklistV (list)
    (setf correct 1)
    (setf hasharray (make-array LIMIT))
    (setf gt 0)
    (loop for cons on list do
        (setq m (first cons))
        (if (not(null (aref hasharray m)))
            (if (not(eq 0 (aref hasharray m)))
                (setf correct 0)
            )
        )
        (setf (aref hasharray m) 1)
        (if (= correct 0)
            (return)
        )
    )
    (if (= correct 1) t nil)
)

;print out the results
(defun printlist (list)
  (format t "(")
  (loop for cons on list do
    (format t "~a" (first cons))
    when (rest cons) do
      (format t ",")
  )
  (format t ")~%")
)

(start)
