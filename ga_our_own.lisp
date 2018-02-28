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
(defvar fitList '())

(defun fitness (AW dataset)
	(loop for x in (2d-array-to-list AW)
		do (setq fitList (append fitList (list (/ 1 (+ 1 (closeness x  dataset) )))))
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
	(print r)
	(print AW)
	(loop for x in fitList
		do((lambda()
				(
					if (> partsum r) 
				(setf i count)
				(progn
					(setf partsum (+ partsum x))
					(incf count)
				))
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
(defun cross-over (AW dataset)
	(loop for x from 1 to 3
		do(
						
				(lambda () (setf p1 (get-list (roulette-wheel (fitList) (2d-array-to-list AW)))))
				(lambda () (setf p2 (get-list (roulette-wheel (fitList) (2d-array-to-list AW))))
				)
				
				(loop for y from 1 to 5
					do( (lambda () 
						(if (oddp y)
							;uniform crossover
							(
								(lambda () (setf c1 (append c1 (car p1))))
								(lambda () (setf c2 (append c2 (car p2)))))
								
							
								((lambda () (setf c1 (append c1 (car p2))))
								(lambda () (setf c2 (append c2 (car p1)))))
								
						)
						
							(lambda () (setf p1 (cdr p1)))
							(lambda () (setf p2 (cdr p2)))
							)										
						)
					)
				
					
				
					(lambda () (setf newpop (append newpop c1)))
						(lambda () (setf newpop (append newpop c2)))
					
				

			;	(lambda ()				
			;	 ((setf p1 (get-list (roulette-wheel (fitList) (2d-array-to-list AW))))
			;	(setf p2 (get-list (roulette-wheel (fitList) (2d-array-to-list AW))))
			;	) 
			;	)

			;	(loop for y from 1 to 5
			;		do(  
			;			(if (oddp y)
							;uniform crossover
			;				((lambda ()
			;					((setf c1 (append c1 (car p1)))
			;					(setf c2 (append c2 (car p2))))
			;					))
			;				((lambda ()
			;					((setf c1 (append c1 (car p2)))
			;					(setf c2 (append c2 (car p1))))
			;					))
			;			)
			;			(lambda () 
			;				((setf p1 (cdr p1))
			;				 (setf p2 (cdr p2))
			;				)										
			;			)
			;		)
			;	)
				

			;	(lambda ()
			;		((setf newpop (append newpop c1))
			;	     (setf newpop (append newpop c2)))
			;		)
			;	


			






		)
			
	)
)


;defun mutation (AW)
;	()
;	)
;(print (wt-sum '(1 2 3 4 5) '(1 2 3 4 5)))
;(print (closeness '(1 2 3 4 5) dataset))
;(print (aref AW 0 0))
(fitness AW dataset)
(print fitList)
(cross-over AW dataset)
(print newpop)
;(print (get-list (roulette-wheel fitList) (2d-array-to-list AW)))