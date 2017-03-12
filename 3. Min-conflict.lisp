(defun makerandomlist (size)
	(setf arr nil)
	(dotimes (a size)
		(setf arr (append arr (cons (random size) nil)))
	)
	arr ;returns a randomly initialized list
)

(defun constraints (olist)
	(setf res (make-list (length olist) :initial-element 0))
	(setf outer 0)
	(loop for cons on olist do
		(setf violations 0)
		(setf m (first cons))
		(setf gt m)
		(setf lt m)
		(setf inner 1)
		(loop for cons on (rest cons) do
			(setf n (first cons))
			(setf gt (+ gt 1))
			(setf lt (- lt 1))
			(cond ((= gt n)
				;(format t "Outer ~s with inner ~s~%" outer (+ outer inner))
				(setf res (setlistval res outer (+ (getlistval res outer) 1)))
				(setf res (setlistval res (+ outer inner) (+(getlistval res (+ outer inner)) 1)))
			))
			(cond ((= lt n)
				;(format t "Outer ~s with inner ~s~%" outer (+ outer inner))(setf res (setlistval res outer (+ (getlistval res outer) 1)))
				(setf res (setlistval res outer (+ (getlistval res outer) 1)))
				(setf res (setlistval res (+ outer inner) (+(getlistval res (+ outer inner)) 1)))
			))
			(cond ((=  m n)
				;(format t "Outer ~s with inner ~s~%" outer (+ outer inner))
				(setf res (setlistval res outer (+ (getlistval res outer) 1)))
				(setf res (setlistval res (+ outer inner) (+(getlistval res (+ outer inner)) 1)))
			))
			(setf inner (+ inner 1))
		)
		(setf outer (+ outer 1))
	)
	res
)

(defun setlistval (olist pos val)
	;bounds check
	(if (> (+ pos 1) (length olist)) nil
	(cond ((not(= pos 0))
					(append (cons (first olist) nil) (setlistval (rest olist) (- pos 1) val))
		  )
		  ((= pos 0)
		  			(setf retlist (append (cons val nil) (rest olist)))
		  )
	)
	)
)

(defun getlistval (olist pos)
	;bounds check
	(if (> (+ pos 1) (length olist)) nil
	(cond ((not(= pos 0))
					(getlistval (rest olist) (- pos 1))
		  )
		  ((= pos 0)
		  			(first olist)
		  )
	)
	)
)

(defun maxvaluepos (olist)
	(setf maxval (first olist))
	(setf pos 0)
	(setf res nil)
	(loop for cons on olist do
		(cond ((> (first cons) maxval )
			(setf maxval (first cons))
			(setf res nil))
		)
		(if (= (first cons) maxval )
			(setf res (append res (cons pos nil)))
		)
		(setf pos (+ pos 1))
	)
	res
)

(defun ritem (olist)
	(setf listsize (length olist))
	(setf chosen (random listsize))
	(setf counter 0)
	(setf res nil)
	(loop for cons on olist do
		(cond ((= counter chosen)
			(setf res (first cons))
			(return))
		)
		(setf counter (+ counter 1))
	)
	res
)

(defun listsum (olist)
	(setf sum 0)
	(loop for cons on olist do
		(setf sum (+ sum (first cons)))
	)
	sum
)

(defun findbetter (olist pos)
	(setf listcopy olist)
	(setf was (getlistval olist pos))
	(setf nowviol nil)
	(setf conssum nil)
	(setf possibleas nil)
	(dotimes (a (length listcopy))
		;don't want to leave it in the same place
		(cond (
				(not(= a was))
				(setf listcopy (setlistval listcopy pos a))
				(if (null conssum)
					(setf conssum (listsum (constraints listcopy)))
				)
				(setf violist (constraints listcopy))
				(setf nowsum (listsum violist))
				(cond (
					  (< nowsum conssum )
					  (setf conssum nowsum)
					  (setf possibleas (cons a nil))
					  )
					  (
					  (= nowsum conssum )
					  (setf possibleas (append possibleas (cons a nil)))
					  )
				)
			)
		)
	)
	;(format t "Possible As : ~s~%" possibleas)
	(setf actual (ritem possibleas))
	(setf listcopy (setlistval listcopy pos actual))
	listcopy
)

;(START)
(defun startEX (&optional x)
	;setting up a field size, if specified
	(setf size 4)
	(cond ((not(null x))(setf size x)))
	;generating a random field of that size
	(setf mylist (makerandomlist size))
	(format t "Starting with a field : ~s~%" mylist)
	;check violated constraints
	(setf cstr (constraints mylist))
	(format t "Constr violated by row: ~s~%" cstr)
	(format t "Total violations : ~s~%" (listsum cstr))
	;finding the rows with maximum violations
	(setf rowstofix (maxvaluepos cstr))
	(format t "Rows w/ max violations: ~s~%" rowstofix)
	;choosing one random from them
	(setf rowtofix (ritem rowstofix))
	(format t "Row randomly chosen  :  ~s~%" rowtofix)
	;find places with least violations
	(setf mylist (findbetter mylist rowtofix))
	(format t "Next better state    :  ~s~%" mylist)
	(format t "Total violations : ~s~%" (listsum (constraints mylist)))

)

(defun start (&optional x)
	;setting up a field size, if specified
	(setf size 4)
	(cond ((not(null x))(setf size x)))
	;generating a random field of that size
	(setf mylist (makerandomlist size))

	(setf i 0)

	(loop do
		;check violated constraints
		(setf cstr (constraints mylist))
		;finding the rows with maximum violations
		(setf rowstofix (maxvaluepos cstr))
		;choosing one random from them
		(setf rowtofix (ritem rowstofix))
		;find places with least violations
		(setf mylist (findbetter mylist rowtofix))

		(setf i (+ i 1))
	while ( /= (listsum (constraints mylist)) 0))


	(format t "Result : ~s~%" mylist)
	(format t "It took ~s nodes to find the result ~%" i)

)

;(CONSTRAINTS (LIST 0 2 3 1))
;(CONSTRAINTS (LIST 0 1 2 3))

;(SETLISTVAL (list 3 8 9 4) 0 1)
;(GETLISTVAL (list 3 8 9 4) 0)

;(MAXVALUEPOS (list 3 8 9 4))
;(RITEM(list 4 5 6 3))

(start 10)
