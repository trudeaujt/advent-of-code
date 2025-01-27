(load "~/code/advent-of-code/src/2024/helpers.lisp")

(defparameter small-input 
"....XXMAS..
.SAMXMS....
...S..A....
..A.A.MS.X.
XMASAMX.MM.
X.....XA.A.
S.S.S.S.SS.
.A.A.A.A.A.
..M.M.M.MM.
.X.X.XMASX.")

(defparameter xmas "XMAS")
(defparameter samx "SAMX")

(defparameter dummy-input 
#((A B C)
  (B C D)
  (C D E)))

"A
BB
CCC
DD
E"

(defun rotate-right (text)
    (let* ((vec (split-sequence:split-sequence #\newline text)) 
           (height (length vec))
           (width (length (first vec))))
        (loop for x upto height
                  do (loop for y upto width
                           do (princ (aref text x))))
        ))

(rotate-right small-input)

(defun rotate-string-45 (input-string)
  (let* ((lines (split-sequence:split-sequence #\Newline input-string))
         (rows (length lines))
         (cols (length (first lines)))
         (diagonal-count (+ rows cols -1))
         (diagonals (make-array diagonal-count :initial-element nil)))
    ;; Initialize diagonals as empty lists
    (dotimes (i diagonal-count)
      (setf (aref diagonals i) '()))
    ;; Populate diagonals directly from the string
    (dotimes (i rows)
      (dotimes (j cols)
        (when (< j (length (nth i lines)))  ; Avoid out-of-bounds
          (let ((char (char (nth i lines) j))
                (diag (+ i j)))
            (push char (aref diagonals diag))))))
    ;; Convert diagonals vector to a list of strings
    (let ((result
           (map 'list
                (lambda (diag) (coerce (reverse diag) 'string))
                diagonals)))
      ;; Join the strings with newline characters
      (apply #'concatenate 'string
             (mapcan (lambda (str) (list str (string #\Newline))) result)))))

;; Example usage
(defparameter my-array
"ABC
BCD
CDE")

(format t "~A~%" (rotate-string-45 my-array))

(defun part1 (text)
    (let ((sum 0)) 
        (labels ((count-xmas (line xmas) 
                             (let ((idx (search xmas line))) 
                                 (when idx 
                                       (incf sum) 
                                       (count-xmas (subseq line (+ 4 idx)) xmas))))) 
            (count-xmas text "XMAS")
            (count-xmas text "SAMX"))
        sum))

(part1 small-input)