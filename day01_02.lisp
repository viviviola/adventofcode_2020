;;; Code for day 1 (problem 2).
;;; 12/02/2020
;;;
;;; Following on from Problem 1, we now consider triples
;;; in the given list that sum up to 2020, and we want to
;;; multiply those together.

;;; The example is the same:
(defparameter *test* '(1721 979 366 299 675 1456))
(defparameter *test-solution* '(979 366 675))
(defparameter *test-product* (apply #'* *test-solution*))

;;; Also from Problem 1, we can use our pairs function here too:
(defun pairs (lst)
  "Return all combinations of pairs of the elements of LST."
  (cond ((null lst) nil)
        ((= (length lst) 1) nil)
        ((= (length lst) 2) (list lst))
        (t (append (mapcar (lambda (elt) (list (car lst) elt)) (cdr lst))
                   (pairs (cdr lst))))))

;;; Using pairs directly, we can define a 'triples' function
;;; that also uses the same recursive idea.
(defun triples (lst)
  "Return all combinations of triples of the elements of LST."
  (cond ((null lst) nil)
        ((= (length lst) 1) nil)
        ((= (length lst) 2) nil)
        ((= (length lst) 3) (list lst))
        (t (append
            (mapcar (lambda (elt) (cons (car lst) elt)) (pairs (cdr lst)))
            (trips (cdr lst))))))

;;; As before, we can try this solution on the test data set to
;;; verify that everything works.
(defvar test-answer (loop for triple in (triples *test*)
                       when (= 2020 (apply #'+ triple))
                       return (list triple (apply #'* triple))))

;;; Again, as expected:
(null (set-exclusive-or (car test-answer) *test-solution*))  ;; T
(= (cadr test-answer) *test-product*)  ;; T

;;; Running on the full data set.
;;; (Downloaded to day01_01_input.txt, the same data as problem 1)
(defvar input-data (with-open-file (stream "day01_01_input.txt")
                     (loop for line = (read-line stream nil)
                        while line
                        collect (parse-integer line))))

(defvar answer (loop for triple in (triples input-data)
                  when (= 2020 (apply #'+ triple))
                  return (list triple (apply #'* triple))))

