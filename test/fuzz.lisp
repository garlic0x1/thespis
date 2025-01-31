(fiasco:define-test-package #:thespis/test/fuzz)
(in-package #:thespis/test/fuzz)

(deftest fuzz-basic-tests (&optional (times 1024))
  "Detect rare race conditions by brute force."
  (dotimes (i times)
    (is (fiasco:run-tests
         :thespis/test/basic
         :stream (make-broadcast-stream)))))

(deftest fuzz-dispatcher-tests (&optional (times 4))
  "Don't try as many reps here because it is too slow."
  (dotimes (i times)
    (is (fiasco:run-tests
         :thespis/test/dispatcher
         :stream (make-broadcast-stream)))))
