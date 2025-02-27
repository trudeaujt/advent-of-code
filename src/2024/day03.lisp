(ql:quickload "cl-ppcre")

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
  (let ((file-input (fetch-puzzle-input year day)))
    (case type
      (:array  (to-array  file-input))
      (:single-string (to-single-string file-input)))))

(defun to-single-string (input)
  (format nil "~{~A~}" input))

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

(defun part1 (text)
  (let ((sum 0))
  (cl-ppcre:do-register-groups ((#'parse-integer x y))
      ("mul\\((\\d{1,3}),(\\d{1,3})\\)" text sum)
    (incf sum (* x y)))))

(equal 188741603 (part1 (get-puzzle-input 2024 3 :single-string)))

(defun part2 (text)
  ;; the string starts in the "do" stage, so we work with it until the first "don't", and parse the string from there.
  (let ((sum 0)
        (offset (length "don't()"))
        (start (search "don't()" text)))
    (incf sum (part1 (subseq text 0 start)))
    (setf text (subseq text (+ start offset)))
    ;; recursively parse each subsection inbetween the "do"s and "don't"s
    (labels ((parse (line)
                    (let ((do-idx (search "do" line))
                          (dont-idx (search "don't()" line))) 
                      (when do-idx 
                            (parse (subseq line (+ dont-idx offset)))
                            (incf sum (part1 (subseq line do-idx dont-idx)))))))
      (parse text))))


(equal 67269798 (part2 (get-puzzle-input 2024 03 :single-string)))
