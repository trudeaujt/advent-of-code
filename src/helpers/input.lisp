(defpackage :helpers.input
  (:use :cl)) ; You can add exported symbols here if needed

(in-package :helpers.input)

(ql:quickload '(dexador cl-json))

(defparameter *aoc-input-base-dir* "inputs/")
(defparameter *aoc-base-url* "https://adventofcode.com/")
;(defparameter *session-token* (uiop:getenv "AOC_SESSION_TOKEN"))
(defparameter *session-token* "53616c7465645f5f79aa6e64302e4bb61d9b0b3914a58fe30f4d60ac14232d234bcd592cfb7b17cf9c8289d9933f392e07010c56f972aa1134543ad47daa221d")

(defun ensure-directory-exists (dir)
  "Ensure that the directory DIR exists, creating it if necessary."
  (unless (uiop:directory-exists-p dir)
    (ensure-directories-exist dir)))

(defun fetch-puzzle-input (year day)
  "Fetch the puzzle input for the given YEAR and DAY from Advent of Code."
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
                body)
              (error "Failed to fetch input for year ~D, day ~D: ~A" year day '(body status)))))))

(defun get-puzzle-input (year day)
  "Get the puzzle input for the given YEAR and DAY, downloading it if necessary."
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
