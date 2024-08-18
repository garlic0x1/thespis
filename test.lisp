(defpackage #:thespis/test
  (:use #:cl #:fiveam #:thespis))
(in-package #:thespis/test)

(def-suite :thespis)
(in-suite :thespis)

(test :counter
  (define-actor counter ((i 0)) (increment)
    (incf i increment))

  (let ((actor (counter)))
    (send actor 1)
    (send actor 3)
    (is (= 5 (ask actor 1)))
    (close-actor actor))

  (let ((actor (counter)))
    (send actor 1)
    (send actor -3)
    (is (= -1 (ask actor 1)))
    (close-actor actor)))

;; This does not work because you cant recursively ask, trying to
;; synchrononize makes it block and not process any messages

;; (test :factorial
;;   (define-actor factorializer () (n)
;;     (format t "factorializing: ~a" n)
;;     (force-output)
;;     (if (= 0 n)
;;         1
;;         (* n (ask self (1- n)))))

;;   (let ((actor (factorializer)))
;;     (= 3628800 (ask actor 10))
;;     (close-actor actor)))
