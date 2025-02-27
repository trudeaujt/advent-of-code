(defparameter *aoc-input-base-dir* "inputs/")
(defparameter *aoc-base-url* "https://adventofcode.com/")
(defparameter *session-token* (uiop:getenv "AOC_SESSION_TOKEN"))

(ql:quickload :dexador)

(defun ensure-directory-exists (dir)
  (unless (uiop:directory-exists-p dir)
    (ensure-directories-exist dir)))

(defun fetch-puzzle-input (year day)
  (let* ((url (format nil "~A~D/day/~D/input" *aoc-base-url* year day))
         (headers `(("Cookie" . ,(format nil "session=~A" *session-token*))))
         (output-dir (merge-pathnames (format nil "~D/" year) *aoc-input-base-dir*))
         (output-file (merge-pathnames (format nil "day~2,'0D.txt" day) output-dir)))
    (ensure-directory-exists output-dir)
    (if (probe-file output-file)
        (uiop:read-file-lines output-file)
        (multiple-value-bind (body status) (dex:get url :headers headers)
          (if (= status 200)
              (progn
                (with-open-file (stream output-file
                                        :direction :output
                                        :if-exists :overwrite
                                        :if-does-not-exist :create)
                  (write-string body stream))
                (format t "Downloaded and saved input for year ~D, day ~D.~%" year day)
                (uiop:split-string body :separator uiop:+lf+))
              (error "Failed to fetch input for year ~D, day ~D: ~A" year day '(body status)))))))

(defun get-puzzle-input (year day &optional (type :single-string))
  (let ((file-input (fetch-puzzle-input year day)))
    (case type
      (:array  (to-array file-input))
      (:single-string (to-single-string file-input))
      (:2d-array (to-2d-array file-input)))))

(defun to-single-string (input)
  (format nil "~{~A~%~}" input))

(defun to-array (input)
  (remove-if (lambda (line) (string= "" line))
             (coerce 
               (reduce 
                 (lambda (acc char)
                   (if (char= char #\Newline)
                       (cons "" acc)
                       (cons (concatenate 'string (first acc) (string char)) (rest acc)))) 
                 input :initial-value '(""))
               'vector)))

(defun to-2d-array (input)
  (let* ((rows (length input))
         (cols (length (first input)))
         (matrix (make-array (list rows cols) :element-type 'character)))
    (loop for i from 0 below rows
          for line in input do
          (loop for j from 0 below cols do
                (setf (aref matrix i j) (aref line j))))
    matrix))
