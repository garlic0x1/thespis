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
    (close-actor actor))
  )

;; (defactor :counter ((i 0)) (increment)
;;   (sleep 1)
;;   (incf i increment))

;; (send :counter 1) ;; instant
;; (send :counter 3) ;; instant
;; (ask :counter 1)  ;; takes 3 seconds and returns 5
;; (close-actor :counter)
