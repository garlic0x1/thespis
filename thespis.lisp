(defpackage #:thespis
  (:use #:cl)
  (:export #:actor                 ; STRUCT
           #:*self*                ; VARIABLE
           #:send                  ; FUNCTION
           #:ask                   ; FUNCTION
           #:close-actor           ; FUNCTION
           #:join-actor            ; FUNCTION
           #:destroy-actor         ; FUNCTION
           #:close-and-join-actors ; FUNCTION
           #:define-actor          ; MACRO
           ;; Actor slots, you probably won't need these
           #:actor-behav           ; ACCESSOR
           #:actor-queue           ; ACCESSOR
           #:actor-lock            ; ACCESSOR
           #:actor-cv              ; ACCESSOR
           #:actor-thread          ; ACCESSOR
           #:actor-open-p          ; ACCESSOR
           #:actor-store           ; ACCESSOR
           ))
(in-package #:thespis)

(defvar *self*)

(defstruct close-signal)

(defstruct async-signal
  (msg nil :type list))

(defstruct sync-signal
  (msg      nil                                    :type list)
  (callback (error ":callback must be specified.") :type function))

(defstruct (actor (:constructor make-actor%))
  (behav  (error ":behav must be specified.") :type function)
  (fail   #'error                             :type function)
  (queue  (queues:make-queue :simple-cqueue)  :type queues:simple-cqueue)
  (open-p t                                   :type boolean)
  (store  nil                                 :type list)
  (lock   (bt2:make-lock)                     :type bt2:lock)
  (cv     (bt2:make-condition-variable)       :type bt2:condition-variable)
  (thread nil                                 :type (or null bt2:thread)))

(defun process-message (actor msg)
  (let ((*self* actor))
    (setf (actor-store actor)
          (multiple-value-list
           (handler-case
               (apply (actor-behav actor) msg)
             (error (c) (funcall (actor-fail actor) c)))))))

(defun run-actor (actor)
  "Run main event loop for actor."
  (loop (bt2:thread-yield)
        (let ((sig (queues:qpop (actor-queue actor))))
          (etypecase sig
            (async-signal
             (process-message actor (async-signal-msg sig)))
            (sync-signal
             (process-message actor (sync-signal-msg sig))
             (funcall (sync-signal-callback sig)))
            (close-signal
             (return-from run-actor (apply #'values (actor-store actor))))
            (null
             (bt2:with-lock-held ((actor-lock actor))
               (bt2:condition-wait (actor-cv actor) (actor-lock actor))))))))

(defun make-actor (behav)
  "Make an actor and run event loop."
  (let ((actor (make-actor% :behav behav)))
    (setf (actor-thread actor) (bt2:make-thread (lambda () (run-actor actor))))
    actor))

(defun send-signal (actor sig)
  (with-slots (queue open-p cv) actor
    (unless open-p (error (format nil "Actor ~w is closed" actor)))
    (queues:qpush queue sig)
    (bt2:condition-notify cv)))

(defun close-actor (actor)
  "Send a close-signal to an actor."
  (send-signal actor (make-close-signal))
  (setf (actor-open-p actor) nil))

(defun join-actor (actor)
  "Wait for an actor to finish computing."
  (bt2:join-thread (actor-thread actor))
  (actor-store actor))

(defun destroy-actor (actor)
  "Immediately destroy an actor's thread."
  (bt2:destroy-thread (actor-thread actor)))

(defun close-and-join-actors (&rest actors)
  (mapcar #'close-actor actors)
  (mapcar #'join-actor actors))

(defun send (actor &rest args)
  "Asyncronously send a message to an actor."
  (send-signal actor (make-async-signal :msg args)))

(defun ask (actor &rest args)
  "Syncronously send a message and await a response from an actor"
  (let* ((lock (bt2:make-lock))
         (cv (bt2:make-condition-variable))
         (callback (lambda () (bt2:condition-notify cv))))
    (send-signal actor (make-sync-signal :msg args :callback callback))
    (bt2:with-lock-held (lock) (bt2:condition-wait cv lock))
    (apply #'values (actor-store actor))))

(defmacro define-actor (name state args &body body)
  "This macro creates a function named ,name that spawns an instance of
the actor.  ,state is a let-list of mutable data owned by the
actor. ,args is a lambda-list to destructure messages with, and ,body
is the code to handle messages."
  `(defun ,name ()
     (let ,state
       (labels ((me ,args ,@body))
         (make-actor #'me)))))
