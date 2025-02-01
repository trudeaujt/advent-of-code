(defparameter *aoc-input-base-dir* "inputs/")
(defparameter *aoc-base-url* "https://adventofcode.com/")
;(defparameter *session-token* (uiop:getenv "AOC_SESSION_TOKEN"))
(defparameter *session-token* "53616c7465645f5fd91fa6ca57bf6db645585807c947bb09187e70733e9b0d8ad59788af016808d57dab7e78e2fb86dc084cea1a8505d242587976a083ca6152")

(defun ensure-directory-exists (dir)
  "Ensure that the directory DIR exists, creating it if necessary."
  (unless (uiop:directory-exists-p dir)
    (ensure-directories-exist dir)))

(ql:quickload :dexador)

(defun fetch-puzzle-input (year day)
  "Fetch the puzzle input for the given YEAR and DAY from Advent of Code."
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

(defun get-puzzle-input (year day &optional (type "string"))
  "Get the puzzle input for the given YEAR and DAY, downloading it if necessary."
  (let ((file-input (fetch-puzzle-input year day)))
    (case type
      (:array  (to-array  file-input))
      (:single-string (to-single-string file-input))
      (:2d-array (to-2d-array file-input)))))

(defun to-single-string (input)
  "Parse a CONS of strings into a single string."
  (format nil "~{~A~}" input))

(defun to-array (input)
  "Parse a multi-line string into an array of lines."
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