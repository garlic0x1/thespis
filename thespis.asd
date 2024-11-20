(asdf:defsystem #:thespis
  :author "garlic0x1"
  :description "Threaded actors for Common Lisp"
  :license "MIT"
  :depends-on (#:bordeaux-threads #:queues.simple-cqueue)
  :components ((:file "package")
               (:file "thespis")))

(asdf:defsystem #:thespis/test
  :depends-on (#:thespis #:fiveam)
  :components ((:file "test")))
