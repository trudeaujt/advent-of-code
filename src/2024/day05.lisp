(load "~/code/advent-of-code/src/2024/helpers.lisp")

(defparameter rules 
 "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13")

(defparameter updates 
 "75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defparameter rule-table
    (let ((rule-table (make-hash-table :test #'equal)))
        (dolist (pair (mapcar (lambda (rule) 
                                  (split-sequence:split-sequence #\| rule)) 
                          (split-sequence:split-sequence #\Newline rules)))
          (push (cadr pair) (gethash (car pair) rule-table)))
       rule-table))

(defparameter update-list
    (let ((update-list (mapcar 
                            (lambda (update) (split-sequence:split-sequence #\, update)) 
                            (split-sequence:split-sequence #\Newline updates))))              
         update-list))

(find "47" (gethash "75" rule-table) :test #'equal)
(type-of (first (gethash "75" rule-table)))

(let* ((updates (first update-list))
       (key (car updates))
       (subsequents (cdr updates))
       (values (gethash key rule-table)))
 (format t "Updates: ~A, key: ~A, subs: ~A, values ~A~%" updates key subsequents values))