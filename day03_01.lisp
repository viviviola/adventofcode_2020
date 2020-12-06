;;; Code for day 3 (problem 1).
;;; 12/05/2020
;;;
;;; Here we are given a piece of a 'toboggan slope' that repeats to
;;; the right, and we need to count the number of trees that are
;;; encountered if we travel at a specific angle.

;;; The example:
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

;;; Starting from the top-left corner and going r3 d1, we encounter
;;; exactly 7 trees.
(defparameter *test-solution* 7)

;;; Now the main tricky part of this problem is incorporating the
;;; looping geography. But we can actually handle that simply with
;;; some modulo arithmetic and make a function that returns the
;;; character at a specified i,j position in the grid (starting from
;;; (0,0) in the upper left):
(defun get-char (grid i j)
  "Return the character at the Ith row, Jth column in the looping GRID."
  (let ((row (elt grid i)))
    (elt row (mod j (length row)))))

;;; We can test that this function works as intended by using it
;;; to print an extended version of the test grid:
(defun grid-row (i num)
  "Return the Ith row of the test grid to a specified NUM of characters."
  (format nil "~{~a~}" (mapcar (lambda (j) (get-char *test* i j))
                               (loop for j from 0 to num collect j))))

(defun print-grid (width)
  "Prints the test grid with a given WIDTH of characters from left to right."
  (format t
          "~{~a~%~}"
          (mapcar (lambda (i) (grid-row i width))
                  (loop for i from 0 below (length *test*) collect i))))

;;; So we're confident that our get-char function works to do what
;;; we want with the looping grid. Now we can get the list of
;;; characters (tree/not tree) along a given trajectory. We won't
;;; worry about making sure the R/D numbers are in lowest terms here
;;; (although that would be wise if we wanted to make sure that a user
;;; could input any trajectory).
(defun trajectory (grid r d)
  "Return the trajectory in the GRID from going right R and down D."
  (let* ((row-index (loop for i from 0 by d below (length grid) collect i))
         (col-index (mapcar (lambda (j) (* r j))
                            (loop for j from 0 below (length row-index)
                               collect j))))
    (mapcar (lambda (i j) (get-char grid i j)) row-index col-index)))

;;; And with that, we have everything we need to count trees ('#')
;;; on the trajectory r3d1 and confirm that there are 7 trees.
(count-if (lambda (char) (equalp char #\#))
          (trajectory *test* 3 1))
  
;;; The last thing to do is read in the grid from the input file and
;;; count trees on the r3d1 trajectory there.
(defvar input-data (with-open-file (stream "day03_01_input.txt")
                     (loop for line = (read-line stream nil)
                        while line
                        collect line)))
(count-if (lambda (char) (equalp char #\#))
          (trajectory input-data 3 1))
