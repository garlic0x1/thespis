(fiasco:define-test-package #:thespis/test/dispatcher
  (:use #:thespis))
(in-package #:thespis/test/dispatcher)

(defun ~= (a b)
  (>= 2 (abs (- a b))))

(deftest test-dispatcher-balanced ()
  "Make sure all the workers share equally for similarly timed jobs."
  (define-actor counter ((c 0)) (increment)
    (sleep 0.025)
    (incf c increment))

  (let ((actor (counter :workers 2)))
    (dotimes (i 40) (send actor 1))
    (close-actor actor)
    (let ((stores (join-actor actor)))
      (is (~= (first stores) (second stores))))))

(deftest test-dispatcher-unbalanced ()
  "Make sure a worker with slow jobs doesn't fill up."
  (define-actor sleeper ((c 0)) (time)
    (sleep time)
    (incf c))

  (let ((actor (sleeper :workers 2)))
    (dotimes (i 5)
      (send (first (thespis::dispatcher-workers actor)) 0.2))
    (dotimes (i 20) (sleep 0.01) (send actor 0))
    (is (equal '((5 20)) (close-and-join-actors actor)))))

(deftest test-dispatcher-registry ()
  "Test registering a dispatcher with a global name."
  (define-actor counter ((c 0)) (increment)
    (incf c increment))

  (counter :name :my-counter :workers 2)
  (send :my-counter 1)
  (is (ask :my-counter 1))
  (join-actor (close-actor :my-counter)))

(deftest test-dispatcher-redefine ()
  (define-actor counter ((c 0)) (increment)
    (incf c increment))

  (counter :name :my-counter :workers 2)
  (ask :my-counter 1)
  (ask :my-counter 1)

  (define-actor counter ((c 0)) (increment)
    (incf c (* 2 increment)))

  (send :my-counter 1)
  (send :my-counter 1)
  (is (= 6 (reduce #'+ (join-actor (close-actor :my-counter))))))
