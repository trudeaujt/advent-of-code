(asdf:load-system "advent-of-code")

(defpackage :day01
  (:use :cl)
  (:import-from :helpers.input :get-puzzle-input)) 

(in-package :day01)

(ql:quickload 'str)

(defun parse-input (input)
    (mapcar (lambda (line) (mapcar #'parse-integer (str:words line))) input))

(defun increasing-or-decreasing (list)
    (or (every (lambda (pair) (< (car pair) (cadr pair)))
               (loop for (a b) on list
                  while b
                  collect (list a b)))
        (every (lambda (pair) (> (car pair) (cadr pair)))
               (loop for (a b) on list
                  while b
                  collect (list a b)))))

(defun within-spec (list)
    (every (lambda (pair)
        (and (<= (abs (- (car pair) (cadr pair))) 3)
             (/= (car pair) (cadr pair)))) 
        (loop for (a b) on list
            while b
            collect (list a b))))

(defun day02 ()
    (let ((input (parse-input (get-puzzle-input 2024 02))))
        (count-if (lambda (line)
            (and (increasing-or-decreasing line)
                 (within-spec line)))
        input)))

(defun increasing-or-decreasing-recursive (list error-budget)
    (labels ((tester (lst budget direction)
             (let ((a (car lst)) (b (cadr lst)))
                  (if (or (equal a nil) (equal b nil) (< budget 0))
                      budget
                  (if (funcall direction a b)
                      (tester (cdr lst) budget direction)
                      (tester (cons a (cddr lst)) (1- budget) direction))))))
    (let ((increasing (tester list error-budget #'<=))
          (decreasing (tester list error-budget #'>=)))
    (if (> increasing decreasing)
        increasing decreasing))))


(defun within-spec-recursive (list error-budget)
    (labels ((tester (lst budget)
                (let ((a (car lst)) (b (cadr lst)))
                    (cond 
                          ((or (null a) (null b) (< budget 0)) budget)
                          ((and (<= (abs (- a b)) 3)
                                (/= a b))
                           (tester (cdr lst) budget))
                          (t (max (tester (cons a (cddr lst)) (1- budget))
                                  (tester (cdr lst) (1- budget))))))))
    (tester list error-budget)))

(within-spec-recursive '(1 7 8 9 10) 1)

(defun day02-part2 ()
    (let ((input test-data))
        (count-if (lambda (line) 
            (let ((error-budget 1))
                (setf error-budget (increasing-or-decreasing-recursive line error-budget))
                (setf error-budget (within-spec-recursive line error-budget))
                (format t "Error budget for ~A is ~A.~%" line error-budget)
                (> error-budget -1)))
        input)))

(defun day02-part2.0 ()
  (let ((input test-data)) ;; Replace `test-data` with the actual input for real use.
    (count-if (lambda (line)
                (let ((valid nil))
                  ;; Check if the original line is valid
                  (when (and (increasing-or-decreasing-recursive line 1)
                             (within-spec-recursive line 1))
                    (setf valid t))
                  ;; If not valid, try removing each level and check
                  (unless valid
                    (dotimes (i (length line))
                      (let ((new-line (remove (nth i line) line :count 1)))
                        (when (and (increasing-or-decreasing-recursive new-line 1)
                                   (within-spec-recursive new-line 1))
                          (setf valid t)))))
                  valid))
              input)))

(day02-part2.0)

(defparameter test-data '((7 6 4 2 1)
(1 2 7 8 9)
(9 7 6 2 1)
(1 3 2 4 5)
(8 6 4 4 1)
(1 3 6 7 9)
(1 5 8 9 10)))

(time (day02))

(defun is-safe (row)
  (let ((inc (loop for i from 0 below (1- (length row))
                   collect (- (nth (1+ i) row) (nth i row)))))
    (or (subsetp inc '(1 2 3) :test #'<=)
        (subsetp inc '(-1 -2 -3) :test #'<=))))

(defun safe-count-without-removal (data)
  (count-if #'is-safe data))

(defun safe-count-with-removal (data)
  (count-if (lambda (row)
              (some (lambda (i)
                      (is-safe (append (subseq row 0 i) (subseq row (1+ i)))))
                    (loop for i from 0 below (length row) collect i)))
            data))

(defun day02-part2 (data)
  (let ((safe-without-removal (safe-count-without-removal data))
        (safe-with-removal (safe-count-with-removal data)))
    (format t "Safe rows without removal: ~A~%" safe-without-removal)
    (format t "Safe rows with removal: ~A~%" safe-with-removal)))

(defparameter test-data '((7 6 4 2 1)
                          (1 2 7 8 9)
                          (9 7 6 2 1)
                          (1 3 2 4 5)
                          (8 6 4 4 1)
                          (1 3 6 7 9)))

(day02-part2 (parse-input (get-puzzle-input 2024 2)))

(defun safe-report? (levels)
  (and (or (apply #'< levels)
           (apply #'> levels))
       (loop for (x y) on levels while y always (<= (abs (- x y)) 3))))

(time (count-if #'safe-report? (parse-input (get-puzzle-input 2024 2))))
