(asdf:defsystem #:thespis
  :author "garlic0x1"
  :description "Threaded actors for Common Lisp"
  :license "MIT"
  :depends-on (#:bordeaux-threads #:queues.simple-cqueue)
  :components ((:file "package")
               (:file "thespis"))
  :in-order-to ((test-op (test-op #:thespis/test))))

(asdf:defsystem #:thespis/test
  :depends-on (#:thespis #:fiasco)
  :components ((:module "test"
                :components ((:file "basic")
                             (:file "fuzz"))))
  :perform (asdf:test-op
            (o c)
            (multiple-value-bind (stat result)
                (uiop:symbol-call :fiasco :run-tests
                                  '(:thespis/test/basic
                                    :thespis/test/fuzz))
              (print result)
              (assert (eql t stat)))))
