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
  (cond ((null i)
        (setf i 0)
        (setf arr nil))
  )
  (cond ;all n values in the list are assigned
        ((= i limit)
        (cond ((checklist arr)
              (printlist arr)
              (setf totalfound (+ totalfound 1)))
        )
        )
        ;list is still not long enough
        ((not (= i limit))
        (dotimes (a limit)
          (setq curr a)
          (setq currlist (cons curr nil))
          (inception (+ i 1) (append arr currlist))
        )
        )
  )
)

;list checking function, compares every
;pair of numbers. if error found, returns nil
(defun checklist (list)
  (setf correct 1)
  (loop for cons on list do
    (setq m (first cons))
    (setq gt m)
    (setq lt m)
    (loop for cons on (rest cons) do
      (setq n (first cons))
      (setf gt (+ gt 1))
      (setf lt (- lt 1))
      (cond ((= gt n) (setf correct 0)(return))
            ((= lt n) (setf correct 0)(return))
            ((= m n) (setf correct 0)(return))
      )
    )
    (cond ((= correct 0) (return)))
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
