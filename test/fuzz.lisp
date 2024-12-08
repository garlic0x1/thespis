(fiasco:define-test-package #:thespis/test/fuzz)
(in-package #:thespis/test/fuzz)

(deftest fuzz-basic-tests (&optional (times 1024))
  "Detect rare race conditions by brute force."
  (dotimes (i times)
    (let ((*standard-output* (make-broadcast-stream)))
      (is (fiasco:run-tests :thespis/test/basic)))))
