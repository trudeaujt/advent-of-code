(defpackage :helpers.input
  (:use :cl))

(in-package :helpers.input)

(ql:quickload '(dexador cl-json))

(defparameter *aoc-input-base-dir* "inputs/")
(defparameter *aoc-base-url* "https://adventofcode.com/")
(defparameter *session-token* (uiop:getenv "AOC_SESSION_TOKEN"))

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
        (progn
          (format t "Input for year ~D, day ~D already downloaded.~%" year day)
          (uiop:read-file-lines output-file))
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

(defun get-puzzle-input (year day)
  (fetch-puzzle-input year day))

(defun to-array (input)
  "Parse a multi-line string into an array of lines."
  (remove-if (lambda (line) (string= "" line))
      (coerce 
    (reduce 
        (lambda (acc char)
          (if (char= char #\Newline)
              (cons "" acc)
              (cons (concatenate 'string (first acc) (string char)) (rest acc)))
          ) 
        input :initial-value '(""))
    'vector)))
