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
(#\M #\. #\. #\A)
(#\A #\. #\. #\M)
(#\S #\. #\. #\X)
))

(defparameter directions '(
(-1 -1) (-1 0) (-1 1)
 (0 -1)         (0 1)
 (1 -1)  (1 0)  (1 1)))

(defparameter xmas "XMAS")

(defun part1 (text)
    (let* ((sum 0)
           (dimensions (array-dimensions text))
           (len (first dimensions))
           (width (second dimensions)))
        (dotimes (x len)
            (dotimes (y width)
                (dolist (dir directions)
                        (let ((new-x (+ x (first dir)))
                              (new-y (+ y (second dir)))
                              (word (make-array 1
                                        :element-type 'character 
                                        :adjustable t 
                                        :fill-pointer 1 
                                        :initial-contents (list (aref text x y)))))
                        (dotimes (iter 3)
                            (unless (or (> 0 new-x)
                                        (> 0 new-y)
                                        (< (- len 1) new-x)
                                        (< (- len 1) new-y))
                                (vector-push-extend (aref text new-x new-y) word)
                                (when debug-mode (format t "Value at new index ~A x ~A, walking from ~A x ~A: ~A [~A]~%" new-x new-y x y (aref text new-x new-y) word))
                                (incf new-x (first dir))
                                (incf new-y (second dir))))
                        (if (string= xmas word) (incf sum)))))
            (when debug-mode (fresh-line)))
        sum))

(defparameter debug-mode t)
(part1 small-input)