(load "~/code/advent-of-code/src/2024/helpers.lisp")

(defparameter small-input #2A(
(#\. #\. #\. #\. #\X #\X #\M #\A #\S #\. #\.)
(#\. #\S #\A #\M #\X #\M #\S #\. #\. #\. #\.)
(#\. #\. #\. #\S #\. #\. #\A #\. #\. #\. #\.)
(#\. #\. #\A #\. #\A #\. #\M #\S #\. #\X #\.)
(#\X #\M #\A #\S #\A #\M #\X #\. #\M #\M #\.)
(#\X #\. #\. #\. #\. #\. #\X #\A #\. #\A #\.)
(#\S #\. #\S #\. #\S #\. #\S #\. #\S #\S #\.)
(#\. #\A #\. #\A #\. #\A #\. #\A #\. #\A #\.)
(#\. #\. #\M #\. #\M #\. #\M #\. #\M #\M #\.)
(#\. #\X #\. #\X #\. #\X #\M #\A #\S #\X #\.)))

(defparameter tiny-input #2A(
(#\X #\M #\A #\S)
(#\. #\. #\. #\M)
(#\. #\. #\. #\A)
(#\. #\. #\. #\X)                             
))

(defparameter directions '(
(-1 -1) (-1 0) (-1 1)
 (0 -1)         (0 1)
 (1 -1)  (1 0)  (1 1)))

(defparameter xmas "XMAS")
(defparameter samx "SAMX")

(defun part1 (text)
    (let* ((sum 0)
           (dimensions (array-dimensions text))
           (len (first dimensions))
           (width (second dimensions)))
        (dotimes (x len)
            (dotimes (y width)
                (dolist (dir directions)
                    (let ((new-x (+ x (first dir)))
                          (new-y (+ y (second dir))))
                        (unless (or (> 0 new-x)
                                    (> 0 new-y)
                                    (> len new-x)
                                    (> len new-y))
                            (format t "Value at new index ~A x ~A, walking from ~A x ~A: ~A " new-x new-y x y (aref text new-x new-y))))))
            (fresh-line))
        sum))

(part1 small-input)
(part1 tiny-input)