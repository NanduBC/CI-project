(defvar dataset '((161.90 164.75 161.65 164.34 163)
(161.95 164.75 161.65 164.34 310)
(158.5 163.89 157.51 162.71 900)
(157.07 157.89 150.24 156.41 530)
(160.29 161 155.03 155.15 930)
(163.085 163.4 159.0685 159.54 440)
(154.83 163.72 154 163.03 940)
(159.1 163.88 156 156.49 320)
(166 166.8 160.1 160.5 50)
(167.165 168.62 166.76 167.78 230)
(166.87 168.4417 166.5 167.43 520)
(165.525 167.37 164.7 166.97 470)
(170.16 170.16 167.07 167.45 420)))

(defvar *powerset* '(1 1 1 1 1))

(setf *random-state* (make-random-state t))

(setf AW (make-array '(14 5)))

(defun rand-val(v)
	(+ 0.0 (random v)))

(dotimes (i 14)
	(dotimes (j 5)
		(setf (aref AW i j) (/ (- 1000 (rand-val 2415)) 1000))
		)
	)


;(defun fx (list)
;	(return (nth 1 list)))

;(defvar ith-row-aw
;	(fx AW))

;(defvar ith-row-ds
;	(fx AW))
;Calcualtes the weighted sum = ax + by + cz + .. so on
(defun wt-sum (ith-row-aw ith-row-ds) 
	(if (not (null ith-row-aw)) 
		(+ (* (car ith-row-aw) (car ith-row-ds)) (wt-sum (cdr ith-row-aw) (cdr ith-row-ds)))
		0
	) 
) 
	

(defvar sum 0)

;Calculates % closeness btw 2 rows of AW matrix
(defun closeness (ith-row-aw dataset)
	;(let (sum-value (wt-sum (fx AW) (fx dataset)))
	;	(cl-val 0)
		(setq sum 0)
	;	((/ (abs (- sum-value (nth 6 (fx dataset)))) (nth 6 (fx dataset))))
	;)
	(loop for x in dataset
		do (setq sum (+ sum (/ (abs (- (wt-sum ith-row-aw x) (nth 3 x))) (nth 3 x)) ))
		)
		(/ sum 13) 
	)
	

	;	(loop for x in dataset
	;	do (setq sum (+ sum (wt-sum ith-row-aw x)))
	;	)
	;	sum
	;)

(defun 2d-array-to-list (array)
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))	

(defun list-to-2d-array (list1)
  (make-array (list (length list1)
                    (length (first list1)))
              :initial-contents list1))

(defvar fitList '())

(defun fitness (AW dataset)
	(setf fitList '())
	(loop for x in (2d-array-to-list AW)
		do (
			setq fitList (append fitList (list (/ 1 (+ 1 (closeness x  dataset) ))))
		)
	)
	;(/ 1 (+ 1 (closeness (list (aref AW i 0) (aref AW i 1) (aref AW i 2) (aref AW i 3) (aref AW i 4))  dataset) ))

)

(setf totalsum 0.0)
(setf partsum 0.0)
(setf count 0)
(setf i 0)
(defun roulette-wheel (fitList)
	(setf totalsum (loop for x in fitList sum x))
	(setf r (rand-val totalsum))
	(setf partsum 0.0)
	(setf count 0)
	(loop for x in fitList
		do(progn

		    (if (> partsum r)
				(setf i count)
				(progn
					(setf partsum (+ partsum x))
					(incf count)
				)
			)

		)
	)
	count
)

(defun get-list (i aw_list)
	(if (= i 1)
		(car aw_list)
		(get-list (decf i) (cdr aw_list))
	)
)

(setf newpop '())
(setf p1 '())
(setf p2 '())
(setf c2 '())
(setf c1 '())

(defun cross-over (AW fitList)
	(dotimes (i 3) (progn
	(setf p1 (get-list (roulette-wheel fitList) (2d-array-to-list AW)))
	(setf p2 (get-list (roulette-wheel fitList) (2d-array-to-list AW)))
	(setf c1 '())
	(setf c2 '())
	(loop for y from 1 to 5
		do(progn
			(if (oddp y)
				;uniform crossover
				(progn
		            (setf c1 (append c1 (list (car p1))))
		            (setf c2 (append c2 (list (car p2))))
		            )


			(progn
				(setf c1 (append c1 (list (car p2))))
				(setf c2 (append c2 (list (car p1))))
			)
			)
			(setf p1 (cdr p1))
			(setf p2 (cdr p2))
			)

		)

		(setf newpop (append newpop (list c1)))
		(setf newpop (append newpop (list c2)))
		)

	)
)	

(defvar newfitList '())

(defun fitness-again (aw_list dataset)
	(setq newfitList '())
	(loop for x in aw_list
		do(
			setf newfitList (append newfitList (list (/ 1 (+ 1 (closeness x  dataset) ))))
		)
	)
)

(defun maxq (list1) 
	(if (eq (length list1) 1)
		(car list1)
		(if (> (car list1) (maxq (cdr list1)))
			(car list1)
			(maxq (cdr list1))
		)
	)
)

(defun minq (list1) 
	(if (eq (length list1) 1)
		(car list1)
		(if (< (car list1) (minq (cdr list1)))
			(car list1)
			(minq (cdr list1))
		)
	)
)

(defun remove-min (list1)
	(
		if (eq (length list1) 1)
		'()
		(
			if (= (car list1) (minq list1))
			(cdr list1)
			(append (list (car list1)) (remove-min (cdr list1)))
		)
	)
)

(defun remove-max (list1)
	(
		if (eq (length list1) 1)
		'()
		(
			if (= (car list1) (maxq list1))
			(cdr list1)
			(append (list (car list1)) (remove-max (cdr list1)))
		)
	)
)

(defun remove-index (i list1)
	(
		if(= i 1)
		(cdr list1)
		(append (list (car list1)) (remove-index (decf i) (cdr list1)))
	)
)

(defun search-index (val list1)
	(
		if(= val (car list1))
		1
		(+ 1 (search-index val (cdr list1)))
	)
)

(defun replace-chromes (aw_list newpop fitList newfitList)
;(loop while (or (not(null newfitList)) (> (maxq newfitList) (minq fitList)))
;do
			( prog2

				(setq aw_list (remove-index (search-index (minq fitList) fitList) aw_list))
				(setq aw_list (append aw_list (list (get-list (search-index (maxq newfitList) newfitList) newpop))))
				(setq newpop (remove-index (search-index (maxq newfitList) newfitList) newpop))
				(setq fitList (remove-index (search-index (minq fitList) fitList) fitList))
				(setq newfitList (remove-index (search-index (maxq newfitList) newfitList) newfitList)))

			(if(or (not(null newfitList)) (> (maxq newfitList) (minq fitList)))
				(prog2

					(setq aw_list (remove-index (search-index (minq fitList) fitList) aw_list))
					(setq aw_list (append aw_list (list (get-list (search-index (maxq newfitList) newfitList) newpop))))
					(setq newpop (remove-index (search-index (maxq newfitList) newfitList) newpop))
					(setq fitList (remove-index (search-index (minq fitList) fitList) fitList))
					(setq newfitList (remove-index (search-index (maxq newfitList) newfitList) newfitList)))
			)
			(if(or (not(null newfitList)) (> (maxq newfitList) (minq fitList)))
				(prog2

					(setq aw_list (remove-index (search-index (minq fitList) fitList) aw_list))
					(setq aw_list (append aw_list (list (get-list (search-index (maxq newfitList) newfitList) newpop))))
					(setq newpop (remove-index (search-index (maxq newfitList) newfitList) newpop))
					(setq fitList (remove-index (search-index (minq fitList) fitList) fitList))
					(setq newfitList (remove-index (search-index (maxq newfitList) newfitList) newfitList)))
			)
			(if(or (not(null newfitList)) (> (maxq newfitList) (minq fitList)))
				(prog2

					(setq aw_list (remove-index (search-index (minq fitList) fitList) aw_list))
					(setq aw_list (append aw_list (list (get-list (search-index (maxq newfitList) newfitList) newpop))))
					(setq newpop (remove-index (search-index (maxq newfitList) newfitList) newpop))
					(setq fitList (remove-index (search-index (minq fitList) fitList) fitList))
					(setq newfitList (remove-index (search-index (maxq newfitList) newfitList) newfitList)))
			)
			(if(or (not(null newfitList)) (> (maxq newfitList) (minq fitList)))
				(prog2

					(setq aw_list (remove-index (search-index (minq fitList) fitList) aw_list))
					(setq aw_list (append aw_list (list (get-list (search-index (maxq newfitList) newfitList) newpop))))
					(setq newpop (remove-index (search-index (maxq newfitList) newfitList) newpop))
					(setq fitList (remove-index (search-index (minq fitList) fitList) fitList))
					(setq newfitList (remove-index (search-index (maxq newfitList) newfitList) newfitList)))
			)
			(if(or (not(null newfitList)) (> (maxq newfitList) (minq fitList)))
				(prog2

					(setq aw_list (remove-index (search-index (minq fitList) fitList) aw_list))
					(setq aw_list (append aw_list (list (get-list (search-index (maxq newfitList) newfitList) newpop))))
					(setq newpop (remove-index (search-index (maxq newfitList) newfitList) newpop))
					(setq fitList (remove-index (search-index (minq fitList) fitList) fitList))
					(setq newfitList (remove-index (search-index (maxq newfitList) newfitList) newfitList)))
			)
			)


;(fitness AW dataset)
;(print fitList)
;(cross-over AW fitList)
;(print newpop)
;(fitness-again newpop dataset)
(defun genetic (AW dataset)

	;(setq newAW (list-to-2d-array (replace-chromes (2d-array-to-list AW) newpop fitList newfitList)))

	(dotimes (i 3)
		(fitness AW dataset)
		(cross-over AW fitList)
		(fitness-again newpop dataset)
		(setq AW (list-to-2d-array (replace-chromes (2d-array-to-list AW) newpop fitList newfitList)))
		;(fitness AW dataset)
		;(print fitList)
		
		)

		)
(genetic AW dataset)

;
;(print c1)
;(print c2)
;(print (get-list (roulette-wheel fitList) (2d-array-to-list AW)))