;;; Code for day 1 (problem 1).
;;; 12/02/2020
;;;
;;; Our task is to read in a list of integers, determine the
;;; two entries (assuming there are only two) that sum to 2020,
;;; and then multiply them together instead.

;;; We are given an example:
(defparameter *test* '(1721 979 366 299 675 1456))

;;; The solution to this test data set should be as follows:
(defparameter *test-solution* '(1721 299))
(defparameter *test-product* (apply #'* *test-solution*))

;;; For a problem domain of this size, it simple and relatively
;;; quick to just enumerate them all and check each pair in turn.
(defun pairs (lst)
  "Return all combinations of pairs of the elements of LST."
  (cond ((null lst) nil)
        ((= (length lst) 1) nil)
        ((= (length lst) 2) (list lst))
        (t (append (mapcar (lambda (elt) (list (car lst) elt)) (cdr lst))
                   (pairs (cdr lst))))))

;;; Then we loop over every pair of integers in the test data set,
;;; and find that pair which sums to 2020.
(defvar test-answer (loop for pair in (pairs *test*)
                       when (= 2020 (apply #'+ pair))
                       return (list pair (apply #'* pair))))

;;; Sure enough:
(null (set-exclusive-or (car test-answer) *test-solution*))  ;; T
(= (cadr test-answer) *test-product*)  ;; T

;;; So we're ready to run our solution on the full data set.
;;; (Downloaded to day01_01_input.txt)
(defvar input-data (with-open-file (stream "day01_01_input.txt")
                     (loop for line = (read-line stream nil)
                        while line
                        collect (parse-integer line))))

(defvar answer (loop for pair in (pairs input-data)
                  when (= 2020 (apply #'+ pair))
                  return (list pair (apply #'* pair))))
