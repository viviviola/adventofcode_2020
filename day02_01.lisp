;;; Code for day 2 (problem 1).
;;; 12/05/2020
;;;
;;; We are given a list of passwords and password policies, and we need to
;;; determine the number of passwords that satisfy the requirements.

;;; The example:
(defparameter *test*
  (list
   '(:password "abcde" :policy "1-3 a")
   '(:password "cdefg" :policy "1-3 b")
   '(:password "ccccccccc" :policy "2-9 c")))

;;; From this test data, exactly 2 passwords should be valid.
(defparameter *test-solution* 2)

;;; For this problem we simply want a way to count the number of times
;;; a given character appears in a string.
(defun char-count (string character)
  "Count the number of times that CHARACTER appears in the STRING."
  (count-if (lambda (char) (equalp char character)) string))

;;; Next we need to parse a policy string. These strings are assumed
;;; to have the form:
;;; (min_count)-(max_count) char
;;; with the min_count and max_count separated by a dash, and a space
;;; separating the counts from the character. We can use regular
;;; expressions for this:
(ql:quickload :cl-ppcre)
(defun parse-policy (string)
  "Return a plist for the min-count/max-count/character in the policy STRING."
  (let* ((policy-plist (mapcan #'list
                              '(:min-count :max-count :character)
                              (cl-ppcre::split "[ -]" string)))
         (min-count (getf policy-plist :min-count))
         (max-count (getf policy-plist :max-count)))
    (setf (getf policy-plist :min-count) (parse-integer min-count))
    (setf (getf policy-plist :max-count) (parse-integer max-count))
    policy-plist))

;;; Now we can test each password for validity.
(defun valid-password-p (password-and-policy)
  "Return T if the given PASSWORD satisfies its POLICY."
  (let* ((password (getf password-and-policy :password))
         (policy (parse-policy (getf password-and-policy :policy)))
         (count (char-count password
                            (character (getf policy :character)))))
    (and (<= (getf policy :min-count) count)
         (<= count (getf policy :max-count)))))

;;; And we're done; we simply use this function to check every password
;;; in the list.
(mapcar #'valid-password-p *test*)
(count-if #'valid-password-p *test*)

;;; Now we need only read in the input list, convert each line to the
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
