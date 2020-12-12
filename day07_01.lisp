;;; Code for day 7 (problem 1).
;;; 12/12/2020
;;;
;;; Here we are given a list of rules for bags (containers), and we
;;; must determine the number of valid outmost bags that may
;;; contain our specific shiny gold bag.

;;; The example:
(defparameter *test*
  (list
   '("light red" . ("bright white"
                    "muted yellow"
                    "muted yellow"))
   '("dark orange" . ("bright white"
                      "bright white"
                      "bright white"
                      "muted yellow"
                      "muted yellow"
                      "muted yellow"
                      "muted yellow"))
   '("bright white" . ("shiny gold"))
   '("muted yellow" . ("shiny gold"
                       "shiny gold"
                       "faded blue"
                       "faded blue"
                       "faded blue"
                       "faded blue"
                       "faded blue"
                       "faded blue"
                       "faded blue"
                       "faded blue"
                       "faded blue"))
   '("shiny gold" . ("dark olive"
                     "vibrant plum"
                     "vibrant plum"))
   '("dark olive" . ("faded blue"
                     "faded blue"
                     "faded blue"
                     "dotted black"
                     "dotted black"
                     "dotted black"
                     "dotted black"))
   '("vibrant plum" . ("faded blue"
                       "faded blue"
                       "faded blue"
                       "faded blue"
                       "faded blue"
                       "dotted black"
                       "dotted black"
                       "dotted black"
                       "dotted black"
                       "dotted black"
                       "dotted black"))
   '("faded blue" . ())
   '("dotted black" . ())))

;;; The solution to the test example is that our shiny gold bag
;;; may be contained in at most 4 other (outmost) bags: a bright
;;; white bag, a muted yellow bag, a dark orange bag, or a light
;;; red bag.
(defparameter *test-solution*
  '("bright white" "muted yellow" "dark orange" "light red"))

;;; We'll represent the bag container rules as alists (and
;;; worry about parsing the input into an alist for later).
;;; In this alist case, we can get the bags that aare contained
;;; by a given bag like this (assuming well-defined input where
;;; each bag has one container rule):
(defun contains-bags (bag rules)
  "The list of bags contained by the given BAG, using the given RULES."
  (cdr (assoc bag rules :test #'string=)))

;;; Of more interest is the bags that directly contain a given bag.
;;; We can get at these in a straight-forward manner using the alist
;;; infrastructure too:
(defun directly-contained-by (bag rules)
  "The bags that directly contain the given BAG, using the given RULES."
  (remove-duplicates
   (loop for rule in rules
      when (member bag (cdr rule) :test #'string=)
      collect (car rule))
   :test #'string=))

;;; This function provides the crux of our program; to find all of
;;; the bags that contain our shiny gold bag at some point, we
;;; want to recursively check whether the shiny gold bag or one
;;; of its containing bags is contained within other bags. If we
;;; keep track of the 'outer bags' at each point, then we stop
;;; the search as soon as we have not added a new bag in the most
;;; recent iteration.
(defun contained-by (bag rules)
  "The bags that (directly or indirectly) contain BAG using the given RULES."
  (defun contained-by-iter (bag-list start-bag)
    (let ((new-bags
           (remove-duplicates
            (append
             (mapcan
              (lambda (bag) (directly-contained-by bag rules))
              (if (null bag-list) (list start-bag) bag-list))
             bag-list))))
      (if (null (set-exclusive-or bag-list new-bags :test #'string=))
          new-bags
          (contained-by-iter new-bags nil))))
  (contained-by-iter nil bag))

;;; And we can check that this gives us exactly the list of bags
;;; that we expect for the test input:
(contained-by "shiny gold" *test*)

;;; Now the last part is the tricky conversion of the input file
;;; into the alist form that we used above. We can do this by
;;; splitting the string and using some basic regular expressions:
(ql:quickload :cl-ppcre)

(defun parse-bag-rule (string)
  "Return a cons cell of the form (container . bags) for the rule in STRING."
  (let ((container-and-bags
         (cl-ppcre::split
          "contain"
          (cl-ppcre::regex-replace-all "bags|bag|[.]" string ""))))
    (cons
     (string-trim " " (car container-and-bags))
     ;; Check for the special case of no other bags
     (if (cl-ppcre::scan "no other" (cadr container-and-bags))
         nil
         (mapcan
          (lambda (string)
            (let* ((bag-and-number
                    (cl-ppcre::split "([\\d+])" string :with-registers-p T))
                   (times (parse-integer (cadr bag-and-number)))
                   (bag (string-trim " " (caddr bag-and-number))))
              (loop for n from 1 to times collect bag)))
          (cl-ppcre::split "," (cadr container-and-bags)))))))

;;; Now we read in the input list and convert each of the rules into
;;; the alist form we use here.
(defvar input-data (with-open-file (stream "day07_01_input.txt")
                     (loop for line = (read-line stream nil)
                        while line
                        collect (parse-bag-rule line))))

(length (contained-by "shiny gold" input-data))
