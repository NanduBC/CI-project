(defvar *genes* '((0 (0 0 0 0))
                  (1 (0 0 0 1))
                  (2 (0 0 1 0))
                  (3 (0 0 1 1))
                  (4 (0 1 0 0))
                  (5 (0 1 0 1))
                  (6 (0 1 1 0))
                  (7 (0 1 1 1))
                  (8 (1 0 0 0))
                  (9 (1 0 0 1))
                  (+ (1 0 1 0))
                  (- (1 0 1 1))
                  (* (1 1 1 1))
                  (/ (1 1 0 1))))

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
(170.16 170.16 167.07 167.96 50565420),
(172 172 170.06 171.51 39075250),
(174.505 174.95 170.53 171.11 41438280),
(177.25 177.3 173.2 174.22 51368540),
(177.3 179.44 176.82 177.04 32395870),
(177.3 177.78 176.6016 177 27052000),
(178.61 179.58 177.41 178.46 31306390),
(179.37 180.1 178.25 179.26 31087330),
(176.15 179.25 175.07 179.1 34260230),
(177.9 179.39 176.14 176.19 29512410),
(176.18 177.36 175.65 177.09 25302200),
(174.59 175.4886 174.49 175.28 18653380),
(173.16 174.3 173 174.29 23751690),
(174.55 175.06 173.41 174.33 21532200),
(174.35 175.61 173.93 174.35 20523870),
(173.44 175.37 173.05 175 23589930),
(172.54 173.47 172.08 173.03 22342650),
(172.53 174.55 171.96 172.23 29461040),
(170.16 172.3 169.26 172.26 25400540),
(170.52 170.59 169.22 169.23 25938760),
(171 171.85 170.48 171.08 16412270),
(170.1 170.78 169.71 170.6 21477380),
(170.8 171.47 169.679 170.57 33113340),
(174.68 175.424 174.5 175.01 16339690),
(174.17 176.02 174.1 175.01 20848660),
(174.87 175.42 173.25 174.35 23451420),
(175.03 175.39 174.09 174.54 27393660),
(174.88 177.2 174.86 176.42 29385650),
(173.63 174.17 172.46 173.97 40122100),
(172.4 173.13 171.65 172.22 20442740),
(172.5 173.54 172 172.27 23402510),
(172.15 172.39 171.461 171.7 19088170),
(169.2 172.89 168.79 172.67 34260890),
(170.49 171 168.82 169.37 23285760),
(169.03 170.44 168.91 169.32 25656110),
(167.5 170.2047 166.46 169.01 28397810),
(169.06 171.52 168.4 169.64 27315530),
(172.48 172.62 169.63 169.8 32512580),
(169.95 171.67 168.5 171.05 39735110),
(170.43 172.14 168.44 171.85 41484540),
(172.63 172.92 167.16 169.48 41459120),
(174.3 174.87 171.86 173.07 26387240),
(175.05 175.08 173.34 174.09 20591090),
(175.1 175.5 174.6459 174.97 14026670),
(173.36 175 173.05 174.96 25558680),
(170.78 173.7 170.78 173.14 25047130),
(170.29 170.56 169.56 169.98 16041550),
(171.04 171.39 169.64 170.15 21884010),
(171.18 171.87 170.3 171.1 23598650),
(169.97 170.3197 168.38 169.08 28998220),
(173.04 173.48 171.18 171.34 24683350),
(173.5 174.5 173.4 173.97 16956290)))

(defvar *mutation-rate* 0.001)
(defvar *crossover-rate* 0.7)

(defun random-item (list)
  "Take a list and return one item from it at random"
  (nth (random (length list)) list))

(defun generate-random-chromosome (size)
  (loop for i from 1 to size
     append (cadr (random-item *genes*))))

(defun decode-gene (gene)
  (car (rassoc gene *genes* :key #'car :test #'equal)))

(defun decode-chromosome (chromosome)
  (remove nil (loop for (a b c d) on chromosome by #'cddddr
                collect (decode-gene (list a b c d)))))

; FIXME x / 0 is silently dropped and operator precedence isn't correct
(defun find-answer (dc)
  "Find the answer given by a decoded-chromosome"
  (cond ((not (consp dc)) dc)
        ((< (list-length dc) 3) (car (remove-if-not 'numberp dc)))
        ((not (numberp (car dc))) (find-answer (cdr dc)))
        ((numberp (cadr dc)) (find-answer (cons (car dc) (cddr dc))))
        ((not (numberp (caddr dc)))
         (find-answer (append (subseq dc 0 2) (cdddr dc))))
        ((and (eql '/ (cadr dc))
              (eql 0 (caddr dc)))
         (find-answer (cons (car dc) (cddr dc))))
        (t
         (let ((simplified (eval (list (cadr dc) (car dc) (caddr dc)))))
           (if (consp (cdddr dc))
               (find-answer (cons simplified (cdddr dc)))
               simplified)))))

;(defun count-flaws (dc)
;  "Return the number of items in a chromosome which are semantically wrong e.g. '(* / +) would be 3"
;  (let ((should-be-number t)
;        (flaw-count 0))
;    (loop for gene in dc
;       if (not (eql (numberp gene) should-be-number))
;       do (incf flaw-count)
;       do (setf should-be-number (if should-be-number nil t)))
;    flaw-count))

; FIXME -100 is given to all symbol chromosomes, probably a bad idea?
(defun fitness (chromosome goal)
  "Return a fitness based on the distance from the answer and the number of flaws"
  (let ((answer (find-answer (decode-chromosome chromosome))))
    (if (numberp answer)
        (let ((distance (abs (- goal answer)))
              (flaws (count-flaws (decode-chromosome chromosome))))
          (+ (if (eql distance 0) 0 (/ 1 distance))
             (if (eql flaws 0) 0 (/ 1 flaws))))
        -100)))

(defun pool-fitness (pool goal)
  (loop for chromosome in pool
     collect (fitness chromosome goal)))

(defun mutate-bit (bit)
  "Take a 1 or 0 and mutate it."
  (if (< (random 1.0) *mutation-rate*)
      (if (eql bit 0) 1 0)
      bit))

(defun mutate (chromosome)
  "Returns a possibly mutated version of chromosome"
  (loop for bit in chromosome
     collect (mutate-bit bit)))

(defun crossover (first second)
  "Returns a mix of two chromosomes (might be the same)"
  (if (< (random 1.0) *crossover-rate*)
      (let ((point (+ (random (- (length first) 1)) 1)))
        (append (subseq first 0 point) 
                (subseq second point)))
      (random-item (list first second))))

(defun make-roulette-wheel (fitness)
  (let* ((total-fitness (reduce #'+ fitness))
         (total-probability 0.0))
    (append (loop for x in fitness
               collect total-probability
               do (incf total-probability (/ x total-fitness))) '(1.0))))
            
(defun spin-the-wheel (pool roulette-wheel)
  (let ((ball (random 1.0)))
    (declare (type float ball))
    (loop for chromosome in pool
       for (position next-pos) of-type (float float) on roulette-wheel
       if (<= ball next-pos)
       do (return chromosome))))
  
(defun re-populate (pool fitness)
  (let ((roulette-wheel (make-roulette-wheel fitness)))
    (loop for i from 1 to (length pool)
       collect (mutate (crossover (spin-the-wheel pool roulette-wheel)
                                  (spin-the-wheel pool roulette-wheel))))))

(defun create-initial-pool (pool-size chromosome-size)
  (loop for i from 1 to pool-size
     collect (generate-random-chromosome chromosome-size)))

(defun find-best-chromosome (pool fitness)
  "Returns the fittest chromosome in the pool."
  (let ((best-score) (best-chromosome))
    (loop for chromosome in pool
       for score in fitness
       do (when (or (equalp score 0) (not best-score) (> score best-score))
            (setf best-score score)
            (setf best-chromosome chromosome)))
    (values best-chromosome best-score)))

(defun there-is-a-winner (pool fitness)
  "If any of the chromosomes in the pool have the answer return the first one that does."
  (let ((winner (position 0 fitness)))
    (if winner (nth winner pool))))

(defun display-turn (pool fitness turn)
  (multiple-value-bind (chromosome score) (find-best-chromosome pool fitness)
    (let ((avg-fitness (/ (reduce #'+ fitness) (+ 1 (length pool))))
          (*print-pretty* nil))
      (format t "~a - Average Fitness: ~F Best: ~w (fitness ~F)~%" turn avg-fitness (decode-chromosome chromosome) score))))

(defun genetic-algorithm (goal pop-size chromosome-size tries)
  (let ((pool (create-initial-pool pop-size chromosome-size))
        (fitness))
    (loop for i from 1 to tries
       do (setf fitness (pool-fitness pool goal))
       if (there-is-a-winner pool fitness) return it
       do (display-turn pool fitness i)
       do (setf pool (re-populate pool fitness))
       finally (return (find-best-chromosome pool fitness)))))

(defun skynet (&key goal pop-size chromosome-size tries mutation-rate crossover-rate)
  (let ((best (genetic-algorithm goal pop-size chromosome-size tries))
        (*print-pretty* nil)
        (*mutation-rate* mutation-rate)
        (*crossover-rate* crossover-rate))
    (format t "~w gave ~F (Aim was ~A)" (decode-chromosome best) (find-answer (decode-chromosome best)) goal)))

; Sensible parameter values according to t'internet:
;   - 0.1 to 0.001 for mutation rate
;   - 0.7 to 0.9 for crossover rate

(skynet :goal 13
        :pop-size 100
        :chromosome-size 9
        :tries 200
        :mutation-rate 0.001
        :crossover-rate 0.7)