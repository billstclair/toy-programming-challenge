;;; excluded-product.lisp
;;; https://kiwifarms.cc/notice/9nMcaMub9cbOys4uG0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Here is another freebie, this time they said it was hard.
;;
;; https://www.dailycodingproblem.com/
;;
;; Good morning! Here's your coding interview problem for today.
;;
;; This problem was asked by Uber.
;;
;; Given an array of integers, return a new array such that each
;; element at index i of the new array is the product of all the
;; numbers in the original array except the one at i.
;;
;; For example, if our input was [1, 2, 3, 4, 5], the expected output
;; would be [120, 60, 40, 30, 24]. If our input was [3, 2, 1], the
;; expected output would be [2, 3, 6].
;;
;; Follow-up: what if you can't use division?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(defun excluded-product (sequence)
  (let ((product (reduce #'* sequence :initial-value 1)))
    (map 'list (lambda (x) (/ product x)) sequence)))

(defun excluded-product-without-division (sequence)
  (flet ((prod (seq)
           (reduce #'* seq :initial-value 1)))
    (loop while (cdr sequence)
       for tail = (cdr sequence) then (cdr tail)
       for head = nil then (append head (list (pop sequence)))
       collect (* (prod head) (prod tail)))))
