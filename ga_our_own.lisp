(defvar *dataset* '((161.90 164.75 161.65 164.34 32549163),
(161.95 164.75 161.65 164.34 32483310),
(158.5 163.89 157.51 162.71 60774900),
(157.07 157.89 150.24 156.41 70583530),
(160.29 161 155.03 155.15 54145930),
(163.085 163.4 159.0685 159.54 51467440),
(154.83 163.72 154 163.03 68171940),
(159.1 163.88 156 156.49 72215320),
(166 166.8 160.1 160.5 85957050),
(167.165 168.62 166.76 167.78 44453230),
(166.87 168.4417 166.5 167.43 32234520),
(165.525 167.37 164.7 166.97 45635470),
(170.16 170.16 167.07 167.96 50565420)))

(defvar *powerset* '(1 1 1 1 1))

(defun fitness ())

(setf *random-state* (make-randon-state t))

(setf AW (make-array '(14 5)))

(defun rand-val()
	(+ 0.0 (random 2415.0)))

(defun fitness ()
	(/ 1 ()))

(defun fx (list)
	(return (nth 1 list)))

;(defvar ith-row-aw
;	(fx AW))

;(defvar ith-row-ds
;	(fx AW))


(defun wt-sum (ith-row-ds ith-row-aw)
	(if (not (null? ith-row-aw))
		(return (+ (* (car (fx ith-row-aw)) (car(fx ith-row-ds))) (closeness (cdr ith-row-aw) (cdr ith-row-ds)))
		(return 0))
	)

(defun closeness (AW dataset)
	(let (sum-value (wt-sum (fx AW) (fx dataset)))
		
		(/ (abs (- sum-value (nth 6 (fx dataset)))) (nth 6 (fx dataset))) ))