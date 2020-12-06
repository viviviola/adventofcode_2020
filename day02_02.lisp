;;; Code for day 2 (problem 2).
;;; 12/05/2020
;;;
;;; We are given a list of passwords and password policies, and we need to
;;; determine the number of passwords that satisfy the requirements.
;;; Part 2 (the requirements have changed, is all).

;;; The example:
(defparameter *test*
  (list
   '(:password "abcde" :policy "1-3 a")
   '(:password "cdefg" :policy "1-3 b")
   '(:password "ccccccccc" :policy "2-9 c")))

;;; For this test data in part 2, exactly 1 password should be valid.
(defparameter *test-solution* 1)

;;; Now we need to confirm that the characters at given positions
;;; (starting from 1, not 0) are as specified. We can write a simple
;;; function to do this:
(defun char-at (string character n)
  "Return T if CHARACTER is at the Nth position in STRING (starting from 1)."
  (equalp character (elt string (1- n))))

;;; Our parsing of the policy strings also changes a bit: the strings
;;; still have the same form, but now they should be interpreted:
;;; (at_position_1)-(at_position_2) char
;;; where char should appear at at_position_1 OR at_position_2, but
;;; not both. parse-policy remains mostly unchanged otherwise:
(ql:quickload :cl-ppcre)
(defun parse-policy (string)
  "Return a plist for the at-pos/not-at-pos/character in the policy STRING."
  (let* ((policy-plist (mapcan #'list
                              '(:at-pos-1 :at-pos-2 :character)
                              (cl-ppcre::split "[ -]" string)))
         (at-pos-1 (getf policy-plist :at-pos-1))
         (at-pos-2 (getf policy-plist :at-pos-2)))
    (setf (getf policy-plist :at-pos-1) (parse-integer at-pos-1))
    (setf (getf policy-plist :at-pos-2) (parse-integer at-pos-2))
    policy-plist))

;;; Our passwords are now valid if they have the given character at
;;; at-pos, and NOT at not-at-pos.
(defun valid-password-p (password-and-policy)
  "Return T if the given PASSWORD satisfies its POLICY."
  (let* ((password (getf password-and-policy :password))
         (policy (parse-policy (getf password-and-policy :policy)))
         (at-pos-1 (char-at password
                            (character (getf policy :character))
                            (getf policy :at-pos-1)))
         (at-pos-2 (char-at password
                            (character (getf policy :character))
                            (getf policy :at-pos-2))))
    (and (or at-pos-1 at-pos-2) (not (and at-pos-1 at-pos-2)))))

;;; And we're done (again); we simply use this function to check
;;; every password in the list.
(mapcar #'valid-password-p *test*)
(count-if #'valid-password-p *test*)

;;; Again we read in the input list, convert each line to the
;;; password-policy plist, and test each password in turn.
(defvar input-data (with-open-file (stream "day02_01_input.txt")
                     (loop for line = (read-line stream nil)
                        while line
                        collect
                          (mapcan
                           #'list
                           '(:policy :password)
                           (cl-ppcre::split ": " line)))))

(count-if #'valid-password-p input-data)
