(load "~/quicklisp/setup.lisp")
(ql:quickload :distributions)

;; Define a list of English letters
(defparameter *eng-letters-list* (coerce "abcdefghijklmnopqrstuvwxyz" 'list))

(format t "~%English letters list:~% ")
(print *eng-letters-list*)
(format t "~%")

;; Generate a random list of 10 numbers from 1 to 1000
(let ((random-state (make-random-state t))) ; Create a new random state
  (defparameter *my-list* (loop repeat 10 collect (random 1000 random-state))))

(format t "~%Input list:~% ")
(print *my-list*)
(format t "~%")

(defparameter *alphabet-power* 3)

(format t "~%Alphabet power:~%")
(print *alphabet-power*)
(format t "~%")

;; Get the sorted list
(defparameter *sort-list* (sort (copy-list *my-list*) #'<))

(format t "~%Sorted list:~%")
(print *sort-list*)
(format t "~%")

;; Get the first element of the list
(defparameter *first-element* (first *sort-list*))

;; Get the last element of the list
(defparameter *last-element* (car (last *sort-list*)))

(defparameter *distrib* (distributions:r-gamma 2 5))

;; (defvar *cdf* (distributions:cdf *distrib* 10))

(defparameter *cdf* (mapcar (lambda (x) (distributions:cdf *distrib* (/ x 1000))) *my-list*))

(format t "~%CDF:~%")
(print *cdf*)
(format t "~%")

(defparameter *interval-indices* (mapcar (lambda (x) (ceiling (* x *alphabet-power*))) *cdf*))

(format t "~%Interval Indices:~%")
(print *interval-indices*)
(format t "~%")

;; Function to convert indices to letters
(defun numbers-to-letters (numbers)
  (mapcar (lambda (num) (nth (- num 1) *eng-letters-list*)) numbers))

(defparameter *letters* (numbers-to-letters *interval-indices*))

(format t "~%Numbers To Letters:~%")
(print *letters*)
(format t "~%")


;; Function to determine the intersections of the matrix
(defun get-cross-of-matrix (current-letter remaining-letters row-index col-index alphabet)
  (cond ((null remaining-letters) 0)
        ((and (equal (nth row-index alphabet) current-letter)
              (equal (nth col-index alphabet) (first remaining-letters)))
         (1+ (get-cross-of-matrix (first remaining-letters) (rest remaining-letters) row-index col-index alphabet)))
        (t (get-cross-of-matrix (first remaining-letters) (rest remaining-letters) row-index col-index alphabet))))

;; Function to output a line of the matrix
(defun get-line-of-matrix (letters alphabet-len row-index alphabet)
  (loop for col-index from 0 below alphabet-len do
        (format t "~a " (get-cross-of-matrix (first letters) (rest letters) row-index col-index alphabet)))
  (format t "~%"))

;; Function to output all lines of the matrix
(defun display-all-lines-of-matrix (letters alphabet-len alphabet)
  (format t "  ")
  (loop for current from 0 below alphabet-len do
        (format t "~a " (nth current alphabet)))
  (format t "~%")

  (loop for current from 0 below alphabet-len do
        (format t "~a " (nth current alphabet))
        (get-line-of-matrix letters alphabet-len current alphabet)))


;; Output the matrix for exponential segments
(format t "~%Matrix: ~%")  ; added a new line here
(display-all-lines-of-matrix *letters* *alphabet-power* *eng-letters-list*)

