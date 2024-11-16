(in-package #:thespis)

(defvar *self*)

(defstruct close-signal)

(defstruct async-signal
  (msg nil :type list))

(defstruct sync-signal
  (msg nil                               :type list)
  (sem (error "Must provide :semaphore") :type bt2:semaphore))

(defstruct (actor (:constructor make-actor%))
  (behav  (error "Must provide :behav") :type function)
  (fail   #'error                       :type function)
  (queue  (error "Must provide :queue") :type q:simple-cqueue)
  (openp  t                             :type boolean)
  (store  nil                           :type list)
  (sem    (error "Must provide :sem")   :type bt2:semaphore)
  (thread nil                           :type (or null bt2:thread)))

(defun process-message (actor msg)
  (let ((*self* actor))
    (setf (actor-store actor)
          (multiple-value-list
           (handler-case
               (apply (actor-behav actor) msg)
             (error (c) (funcall (actor-fail actor) c)))))))

(defun run-actor (actor)
  "Run main event loop for actor."
  (loop
    (let ((sig (q:qpop (actor-queue actor))))
      (etypecase sig
        (async-signal
         (process-message actor (async-signal-msg sig)))
        (sync-signal
         (process-message actor (sync-signal-msg sig))
         (bt2:signal-semaphore (sync-signal-sem sig)))
        (close-signal
         (return-from run-actor))
        (null
         (bt2:wait-on-semaphore (actor-sem actor)))))))

(defun make-actor (behav)
  "Make an actor and run event loop."
  (let ((actor (make-actor% :behav behav
                            :sem (bt2:make-semaphore)
                            :queue (q:make-queue :simple-cqueue))))
    (setf (actor-thread actor) (bt2:make-thread (lambda () (run-actor actor))))
    actor))

(defun send-signal (actor sig)
  (unless (actor-openp actor)
    (error (format nil "Message sent to closed actor: ~w" actor)))
  (q:qpush (actor-queue actor) sig)
  (bt2:signal-semaphore (actor-sem actor)))

(defun close-actor (actor)
  "Send a close-signal to an actor."
  (send-signal actor (make-close-signal))
  (setf (actor-openp actor) nil))

(defun join-actor (actor)
  "Wait for an actor to finish computing."
  (bt2:join-thread (actor-thread actor))
  (apply #'values (actor-store actor)))

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
  (let ((sem (bt2:make-semaphore)))
    (send-signal actor (make-sync-signal :msg args :sem sem))
    (bt2:wait-on-semaphore sem)
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
