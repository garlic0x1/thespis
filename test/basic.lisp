(fiasco:define-test-package #:thespis/test/basic
  (:use #:thespis))
(in-package #:thespis/test/basic)

(deftest test-counter ()
  (define-actor counter ((c 0)) (increment)
    (incf c increment))

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

(deftest test-lambda-rest ()
  (define-actor square-summer ((c 0)) (&rest args)
    (incf c (apply #'+ (mapcar (lambda (x) (* x x)) args))))

  (let ((actor (square-summer)))
    (is (= 5 (ask actor 1 2)))
    (is (= 21 (ask actor 4)))
    (close-actor actor)))

(deftest test-lambda-key ()
  (define-actor point-actor ((x 0) (y 0) (z 0))
      (&key (dx 0) (dy 0) (dz 0))
    (list
     :x (incf x dx)
     :y (incf y dy)
     :z (incf z dz)))

  (let ((actor (point-actor)))
    (send actor :dx -1 :dz 3)
    (is (equal '(:x -1 :y 0 :z 3) (ask actor)))
    (send actor :dy 100)
    (is (equal '(:x -1 :y 100 :z 3) (ask actor)))
    (close-actor actor)))

(deftest test-multiple-values ()
  (define-actor multivaluer ((prev 0)) (next)
    (multiple-value-prog1 (values prev next)
      (setf prev next)))

  (let ((actor (multivaluer)))
    (send actor 1)
    (is (equal '(1 2) (multiple-value-list (ask actor 2))))
    (send actor 1)
    (close-actor actor)
    (is (equal '(2 1) (multiple-value-list (join-actor actor))))))

(deftest test-pong ()
  (let (pinger ponger (result 0))
    (define-actor pinger () (c)
      (incf result)
      (if (< c 10)
          (send ponger (1+ c))
          (progn (close-actor ponger)
                 (close-actor pinger))))

    (define-actor ponger () (c)
      (incf result)
      (if (< c 10)
          (send pinger (1+ c))
          (progn (close-actor ponger)
                 (close-actor pinger))))

    (setf ponger (ponger) pinger (pinger))
    (send ponger 0)
    (join-actor ponger)
    (join-actor pinger)
    (is (= 11 result))))

(deftest test-self ()
  (let ((result 0))
    (define-actor selfish-counter () ()
      (incf result)
      (if (< 10 result)
          (close-actor *self*)
          (send *self*)))

    (let ((actor (selfish-counter)))
      (send actor)
      (join-actor actor)
      (is (= result 11)))))

(deftest test-error-handling ()
  (define-actor failer () (x)
    (/ 1 x))

  (let ((actor (failer)))
    (is (= 1/3 (ask actor 3)))
    (setf (thespis::actor-fail actor)
          (lambda (c) (declare (ignore c)) :failed))
    (is (eq :failed (ask actor 0)))
    (close-actor actor)))

(deftest test-closed-actor ()
  (define-actor closer () (x)
    x)

  (let ((actor (closer)))
    (close-actor actor)
    (handler-case (send actor 'x)
      (error (c) (is (typep c 'simple-error))))))

(deftest test-redefine-actor ()
  (define-actor counter ((c 0)) (increment)
    (incf c increment))

  (let ((actor (counter)))
    (ask actor 1)
    (define-actor counter ((c 0)) (increment)
      (incf c (1- increment)))
    (send actor 3)
    (is (= 3 (ask actor 1)))
    (define-actor counter ((c 32)) (increment times)
      (incf c (* times increment)))
    (is (= 10 (ask actor 1 7)))
    (close-actor actor)))

(deftest test-registry ()
  (define-actor counter ((c 0)) (increment)
    (incf c increment))

  (counter :name :my-counter)
  (send :my-counter 1)
  (is (= 3 (ask :my-counter 2)))
  (close-actor :my-counter))

;; TODO sometimes fails checking registry after join.
;; not really a big deal but it should be fixed.

;; I need to create a Join Sync signal I think...

;; (deftest test-close-and-join ()
;;   (define-actor counter ((c 0)) (increment)
;;     (incf c increment))

;;   (counter :name :my-counter)
;;   (send :my-counter 1)
;;   (is (= 3 (ask :my-counter 2)))
;;   (close-and-join-actors :my-counter)
;;   (is (eql nil (gethash :my-counter *registry*))))
