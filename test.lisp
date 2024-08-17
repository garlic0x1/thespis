(defpackage #:thespis/test
  (:use #:cl #:fiveam))
(in-package #:thespis/test)

(def-suite :thespis)
(in-suite :thespis)

(test :counter
  (thespis:defactor :counter ((i 0)) (increment)
    (incf i increment))
  (thespis:send :counter 1)
  (thespis:send :counter 3)
  (is (= 5 (thespis:ask :counter 1)))
  (thespis:close-actor)
  )

(defactor :counter ((i 0)) (increment)
  (sleep 1)
  (incf i increment))

(send :counter 1) ;; instant
(send :counter 3) ;; instant
(ask :counter 1)  ;; takes 3 seconds and returns 5
(close-actor :counter)
