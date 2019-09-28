;; zero-to-nine.lisp

;; From:
;; https://kiwifarms.cc/notice/9nJp4P3q6BpBBswsiG
;; #toyprogrammingchallenge

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Here's one I found for you :) I was looking through some and it
;; seemed like it might be too much to ask for a fresh beginner, but I
;; will be giving it a try, though I haven't quite yet.
;;
;; The number 1 2 3 4 5 6 7 8 9 can have either the operator "+" or
;; "-" placed between them, as well if no operator is placed then the
;; adjoining numbers go together such as 1 2 would become "12" or 1 2
;; 3 would become "123" and so forth. The goal is to find the
;; combinations that would make the line calculate up to 100 exactly.
;;
;; Here is an example: 12 + 3 - 4 + 5 + 67 + 8 + 9 = 100
;;
;; There are more to be found.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(defvar *operators* '(nil + -))

(defvar *numbers* '(1 2 3 4 5 6 7 8 9))

(defun permutations (operators length)
  "Return all the permutations of `length` selections from the list of `operators`"
  (let ((res (list nil))
        (indices (loop for i from 0 below (length operators) collect i)))
    (dotimes (i length)
      (loop with subres = nil
         for index in indices
         do
           (loop for l in res do
                (push (cons index l) subres))
         finally
           (setf res (nreverse subres))))
    (loop for l in res
       collect (loop for index in l collect (elt operators index)))))

(defvar *permutations* (permutations *operators* (1- (length *numbers*))))

(defun distribute (operators numbers)
  "Return alternating elements of `numbers` and `operators`.
Doesn't make sense unless there is one more number than operator."
  (loop with res = (list (pop numbers))
     for op in operators
     for num in numbers do
       (push op res)
       (push num res)
     finally
       (return (nreverse res))))

(defun scrunch (expression)
  "Combine numbers separated by NIL in expression."
  (loop with last-num = (pop expression)
     with res = nil
     while expression
     for operator = (pop expression)
     for num = (pop expression)
     do
       (cond ((null operator)
              (setf last-num (+ (* 10 last-num) num)))
             (t (push last-num res)
                (push operator res)
                (setf last-num num)))
     finally
       (push last-num res)
       (return (nreverse res))))

(defun expressions (operators numbers)
  "Return all the possible expressions with `operators` between `numbers`."
  (mapcar (lambda (ops) (scrunch (distribute ops numbers)))
          (permutations operators (1- (length numbers)))))

(defvar *expressions*
  (expressions *operators* *numbers*))

(defun evaluate (expression)
  "Evaluate an expression to a number."
  (loop with res = (or (pop expression) 0)
     while expression
     for operator = (pop expression)
     for num = (pop expression)
     do
       (setf res (funcall operator res num))
     finally
       (return res)))

(defun evaluations (operators numbers)
  "Return a list of lists of (<result> <expression>)."
  (sort (mapcar (lambda (expression)
                  (list (evaluate expression) expression))
                (expressions operators numbers))
        #'<
        :key #'car))

(defvar *evaluations*
  (evaluations *operators* *numbers*))

(defun evaluations-to (to &optional
                            (operators *operators*)
                            (numbers *numbers*))
  "Return a list of the <expressions> whose <result> is `to`."
  (mapcar #'second
          (remove-if (lambda (ev) (not (eql (car ev) to)))
                     (evaluations operators numbers))))

(defun print-evaluations-to (to &optional
                                  (operators *operators*)
                                  (numbers *numbers*))
  "Print the result of `evaluations-to`."
  (loop for expression in (evaluations-to to operators numbers)
     do
       (loop for tok in expression
          with first = t
          do
            (if first
                (setf first nil)
                (write-string " "))
            (format t "~a" tok))
       (terpri)))
  
;; The problem was to find the expressions which evaluate to 100.
(print-evaluations-to 100)

#|

123 + 45 - 67 + 8 - 9
123 + 4 - 5 + 67 - 89
123 - 45 - 67 + 89
123 - 4 - 5 - 6 - 7 + 8 - 9
12 + 3 + 4 + 5 - 6 - 7 + 89
12 + 3 - 4 + 5 + 67 + 8 + 9
12 - 3 - 4 + 5 - 6 + 7 + 89
1 + 23 - 4 + 56 + 7 + 8 + 9
1 + 23 - 4 + 5 + 6 + 78 - 9
1 + 2 + 34 - 5 + 67 - 8 + 9
1 + 2 + 3 - 4 + 5 + 6 + 78 + 9

|#
