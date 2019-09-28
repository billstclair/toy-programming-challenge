(in-package :cl-user)

;; Ninety-nine bottle of beer on the wall song generator.
;;
;; My solution to https://kiwifarms.cc/notice/9n0MsrS2Xh4NNLMh7Y
;;
;; Start time: 2:42
;; End time:  2:56

(defun beer (&optional (start 99))
  (loop for num = start then next-num
     for next-num = (1- num)
     for numstr = (if (eql 0 num) "no" (format nil "~r" num)) then next-numstr
     for next-numstr = (if (eql 0 next-num) "no" (format nil "~r" next-num))
     for bottles = (format nil "bottle~p" num)
     for next-bottles = (format nil "bottle~p" next-num)
     while (> num 0) do
       (unless (eql num start) (terpri))
       (format t
               "~@(~a~) ~a of beer on the wall,~%~
                ~a ~a of beer. Take one down,~%~
                pass it around... ~@(~a~) ~a of beer.~%"
               numstr bottles numstr bottles next-numstr next-bottles)))

;; To print the song:
;; (beer)
