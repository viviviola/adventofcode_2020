;;; Code for day 3 (problem 1).
;;; 12/05/2020
;;;
;;; Here we are given a piece of a 'toboggan slope' that repeats to
;;; the right, and we need to count the number of trees that are
;;; encountered if we travel at a specific angle.

;;; In this second part of the problem, we simply have more
;;; trajectories to count trees along. We've already done the hard
;;; work in problem 1, so we'll import that code here and make the
;;; changes.

(defparameter *test*
  (list
   "..##......."
   "#...#...#.."
   ".#....#..#."
   "..#.#...#.#"
   ".#...##..#."
   "..#.##....."
   ".#.#.#....#"
   ".#........#"
   "#.##...#..."
   "#...##....#"
   ".#..#...#.#"))

(defparameter *test-trajectories*
  (list
   '(1 1)
   '(3 1)
   '(5 1)
   '(7 1)
   '(1 2)))

(defparameter *test-solution*
  (list 2 7 3 4 2))

(defun get-char (grid i j)
  "Return the character at the Ith row, Jth column in the looping GRID."
  (let ((row (elt grid i)))
    (elt row (mod j (length row)))))

(defun trajectory (grid r d)
  "Return the trajectory in the GRID from going right R and down D."
  (let* ((row-index (loop for i from 0 by d below (length grid) collect i))
         (col-index (mapcar (lambda (j) (* r j))
                            (loop for j from 0 below (length row-index)
                               collect j))))
    (mapcar (lambda (i j) (get-char grid i j)) row-index col-index)))

;;; Now we simply compute all of the trajectories at once, count their
;;; trees and multiply together:
(apply #'*
       (mapcar (lambda (trajectory)
                 (count-if (lambda (char) (equalp char #\#))
                           trajectory))
               (apply #'mapcar
                      (lambda (r d) (trajectory *test* r d))
                      (apply #'mapcar #'list *test-trajectories*))))

;;; We get 336 (as hoped), so we can apply this same expression to
;;; the input data instead to get the problem answer.
(defvar input-data (with-open-file (stream "day03_01_input.txt")
                     (loop for line = (read-line stream nil)
                        while line
                        collect line)))
(apply #'*
       (mapcar (lambda (trajectory)
                 (count-if (lambda (char) (equalp char #\#))
                           trajectory))
               (apply #'mapcar
                      (lambda (r d) (trajectory input-data r d))
                      (apply #'mapcar #'list *test-trajectories*))))
