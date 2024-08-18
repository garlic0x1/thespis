(defpackage #:thespis
  (:use #:cl)
  (:export #:actor
           #:self
           #:send
           #:ask
           #:close-actor
           #:join-actor
           #:destroy-actor
           #:close-and-join-actors
           #:define-actor
           ;; Actor slots, you probably won't need these
           #:actor-behav
           #:actor-queue
           #:actor-lock
           #:actor-cv
           #:actor-thread
           #:actor-openp
           #:actor-store))
(in-package #:thespis)

(defstruct close-signal)

(defstruct async-signal
  (msg nil :type list))

(defstruct sync-signal
  (msg      nil                                    :type list)
  (callback (error ":callback must be specified.") :type function))

(defstruct (actor (:constructor make-actor%))
  (behav (error ":behav must be specified.") :type function)
  (queue (queues:make-queue :simple-cqueue)  :type queues:simple-cqueue)
  (openp t                                   :type boolean)
  (store nil                                 :type list)
  (lock  (bt2:make-lock)                     :type bt2:lock)
  (cv    (bt2:make-condition-variable)       :type bt2:condition-variable)
  (thread nil                                :type (or null bt2:thread)))

(defun run-actor (actor)
  "Run main event loop for actor."
  (with-slots (behav queue store lock cv) actor
    (loop
      (bt2:thread-yield)
      (let ((sig (queues:qpop queue)))
        (etypecase sig
          (async-signal
           (setf (actor-store actor)
                 (multiple-value-list (apply behav (async-signal-msg sig)))))
          (sync-signal
           (setf (actor-store actor)
                 (multiple-value-list (apply behav (sync-signal-msg sig))))
           (funcall (sync-signal-callback sig)))
          (close-signal
           (return store))
          (null
           (bt2:with-lock-held (lock)
             (bt2:condition-wait cv lock))))))))

(defun make-actor (behav)
  "Make an actor and run event loop."
  (let ((actor (make-actor% :behav behav)))
    (setf (actor-thread actor) (bt2:make-thread (lambda () (run-actor actor))))
    actor))

(defun send-signal (actor sig)
  (with-slots (queue openp cv) actor
    (unless openp (error (format nil "Actor ~w is closed" actor)))
    (queues:qpush queue sig)
    (bt2:condition-notify cv)))

(defun close-actor (actor &aux (actor (resolve-actor actor)))
  "Send a close-signal to an actor."
  (send-signal actor (make-close-signal))
  (setf (actor-openp actor) nil))

(defun join-actor (actor)
  "Wait for an actor to finish computing."
  (etypecase actor
    (symbol (join-actor (gethash actor *actors*)))
    (actor
     (bt2:join-thread (actor-thread actor))
     (actor-store actor))))

(defun destroy-actor (actor)
  "Immediately destroy an actor's thread."
  (etypecase actor
    (symbol (destroy-actor (gethash actor *actors*)))
    (actor (bt2:destroy-thread (actor-thread actor)))))

(defun close-and-join-actors (&rest actors)
  (mapcar #'close-actor actors)
  (mapcar #'join-actor actors))

(defun send (actor &rest args)
  "Asyncronously send a message to an actor."
  (etypecase actor
    (symbol (apply #'send (cons (gethash actor *actors*) args)))
    (actor  (send-signal actor (make-async-signal :msg args)))))

(defun ask (actor &rest args)
  "Syncronously send a message and await a response from an actor"
  (etypecase actor
    (symbol (apply #'ask (cons (gethash actor *actors*) args)))
    (actor
     (let* ((lock (bt2:make-lock))
            (cv (bt2:make-condition-variable))
            (callback (lambda () (bt2:condition-notify cv))))
       (send-signal actor (make-sync-signal :msg args :callback callback))
       (bt2:with-lock-held (lock) (bt2:condition-wait cv lock))
       (apply #'values (actor-store actor))))))

(defmacro define-actor (name state args &body body)
  `(defun ,name ()
     (let ((self nil) ,@state)
       (labels ((me ,args ,@body))
         (setf self (make-actor #'me))))))
