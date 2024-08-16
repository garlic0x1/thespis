(asdf:defsystem #:thespis
  :author "garlic0x1"
  :description "Threaded actors for Common Lisp"
  :license "MIT"
  :depends-on (#:alexandria #:bordeaux-threads #:queues.simple-cqueue)
  :components ((:file "thespis")))
