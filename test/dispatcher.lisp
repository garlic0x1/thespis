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
      (send (first (thespis::dispatcher-workers actor)) 5))
    (dotimes (i 19) (sleep 0.001) (send actor 0))
    (is (= 20 (ask actor 0)))
    (destroy-actor actor)))

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

(deftest test-dispatcher-close-and-join ()
  (define-actor counter ((c 0)) (increment)
    (incf c increment)
    (counter :name :my-counter)
    (send :my-counter 1)
    (close-and-join-actors :my-counter)
    (is (eql nil (gethash :my-counter *registry*)))))

(deftest test-closed-actor ()
  "Sending messages to a closed dispatcher signals an error."
  (define-actor closer () (x)
    x)

  (let ((actor (closer :workers 2)))
    (close-actor actor)
    (signals simple-error
      (send actor 'x))))

(deftest test-dispatcher-multiple-values ()
  "Return multiple values on join."
  (define-actor multivaluer ((prev 0)) (next)
    (multiple-value-prog1 (values prev next)
      (setf prev next))

    (let ((actor (multivaluer :workers 2)))
      (send actor 1)
      (is (= 2 (length (multiple-value-list (ask actor 2)))))
      (is (= 2 (length (multiple-value-list (ask actor 2)))))
      (is (= 2 (length (multiple-value-list (ask actor 2)))))
      (close-actor actor))))

(deftest test-dispatcher-lambda-key ()
  "Make sure lambda lists work."
  (define-actor point-actor ((x 0) (y 0) (z 0))
      (&key (dx 0) (dy 0) (dz 0))
    (list
     :x (incf x dx)
     :y (incf y dy)
     :z (incf z dz)))

  (let ((actor (point-actor :workers 2)))
    (is (equal '(:x -1 :y 0 :z 3) (ask actor :dx -1 :dz 3)))
    (ask actor)
    (close-and-join-actors actor)))

(deftest test-dispatcher-destroy ()
  "Make sure destroy works on globally registered dispatchers."
  (define-actor echoer () (x) x)

  (echoer :name :echoer)
  (is (= 1 (ask :echoer 1)))
  (destroy-actor :echoer)
  (is (null (gethash :echoer *registry*))))
