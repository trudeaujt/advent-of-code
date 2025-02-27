(load "~/code/advent-of-code/src/2024/helpers.lisp")
(load "~/sync/advent-of-code/src/2024/helpers.lisp")

(defun split-string-on-double-newline (str)
  (let ((delim '(#\Newline #\Newline)))
    (let ((result '())
          (start 0))
      (loop for pos = (search delim str :start2 start)
            while pos do
            (push (subseq str start pos) result)
            (setf start (+ pos (length delim))))
      (push (subseq str start) result)
      (nreverse result))))

(defun valid-updatep (rule-table update)
  (let ((valid-update t))
    (loop while update
          do (let* ((key (car update))
                    (subsequent-updates (cdr update))
                    (subsequent-rules (gethash key rule-table)))
               (unless (subsetp subsequent-updates subsequent-rules :test #'equal)
                 (setf valid-update nil))
               (setf update (cdr update))))
    valid-update))

(defun part1 (input)
  (let* ((rules (first (split-string-on-double-newline input)))
         (updates (second (split-string-on-double-newline input)))
         (rule-table (make-hash-table :test #'equal))
         (update-list (mapcar (lambda (update)
                                (split-sequence:split-sequence #\, update))
                              (split-sequence:split-sequence #\Newline updates)))
         (rolling-sum 0))
    ;; add the rules to the hash table
    (dolist (pair (mapcar (lambda (rule) 
                            (split-sequence:split-sequence #\| rule)) 
                          (split-sequence:split-sequence #\Newline rules)))
      (push (cadr pair) (gethash (car pair) rule-table)))
    ;; collect valid updates
    (dolist (update update-list)
      (let ((midpoint (elt update (floor (length update) 2))))
        (when (and
                (valid-updatep rule-table update) 
                (/= 0 (length midpoint))) 
          (incf rolling-sum (parse-integer midpoint)))))
    rolling-sum))

(equal 6951 (part1 (get-puzzle-input 2024 05 :single-string)))
