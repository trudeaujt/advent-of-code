(asdf:load-system "advent-of-code")

(defpackage :day01
  (:use :cl)
  (:import-from :helpers.input :get-puzzle-input :to-array)) ; Import specific symbols from the helpers.input package

(in-package :day01)

(ql:quickload 'str)

(defun day01 ()
  (let* ((input (get-puzzle-input 2024 1))
         (right-hash (make-hash-table))
         (left '()) (right '()))
    (loop for pair in input
          for words = (str:words pair)
          for lnum = (parse-integer (car words))
          for rnum = (parse-integer (cadr words))
          do (incf (gethash rnum right-hash 0))
             (push lnum left)
             (push rnum right))
    (setf left (sort left #'<))
    (setf right (sort right #'<))
    (format T "Day 1 Part 1: ~A~%" ;; Absolute distance between each pair.
      (reduce #'+ (mapcar (lambda (x y) (abs (- x y))) right left)))
    (format T "Day 1 Part 2: ~A~%" ;; Times each left element appears on the right, times that element.
      (reduce (lambda (sum num) (+ sum (* (gethash num right-hash 0) num)))
          left :initial-value 0))))

(time (day01))