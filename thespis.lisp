(in-package #:thespis)

(defvar *self*)

(defvar *registry* (make-hash-table))

(defstruct close-signal)

(defstruct async-signal
  (msg nil :type list))

(defstruct sync-signal
  (msg    nil                           :type list)
  (sem    (error "Must provide :sem")   :type bt2:semaphore))

(defstruct actor
  (behav  (error "Must provide :behav") :type symbol)
  (name   nil                           :type (or nil symbol keyword))
  (fail   #'error                       :type function)
  (queue  (q:make-queue :simple-cqueue) :type q:simple-cqueue)
  (openp  t                             :type boolean)
  (store  nil                           :type list)
  (sem    (bt2:make-semaphore)          :type bt2:semaphore)
  (thread nil                           :type (or null bt2:thread)))

(defun resolve-actor (actor)
  (etypecase actor
    (actor actor)
    (symbol (gethash actor *registry*))))

(defun process-message (actor msg)
  (let ((*self* actor))
    (setf (actor-store actor)
          (multiple-value-list
           (handler-case (apply (actor-behav actor) msg)
             (error (c) (funcall (actor-fail actor) c)))))))

(defun run-actor (actor)
  "Run main event loop for actor."
  (loop (let ((sig (q:qpop (actor-queue actor))))
          (etypecase sig
            (async-signal
             (process-message actor (async-signal-msg sig)))
            (sync-signal
             (process-message actor (sync-signal-msg sig))
             (bt2:signal-semaphore (sync-signal-sem sig)))
            (close-signal
             (remhash (actor-name actor) *registry*)
             (return-from run-actor))
            (null
             (bt2:wait-on-semaphore (actor-sem actor)))))))

(defun send-signal (actor sig)
  (unless (actor-openp actor)
    (error (format nil "Message sent to closed actor: ~w" actor)))
  (q:qpush (actor-queue actor) sig)
  (bt2:signal-semaphore (actor-sem actor)))

(defun close-actor (actor &aux (actor (resolve-actor actor)))
  "Send a close-signal to an actor."
  (send-signal actor (make-close-signal))
  (setf (actor-openp actor) nil))

(defun join-actor (actor &aux (actor (resolve-actor actor)))
  "Wait for an actor to finish computing."
  (bt2:join-thread (actor-thread actor))
  (apply #'values (actor-store actor)))

(defun destroy-actor (actor &aux (actor (resolve-actor actor)))
  "Immediately destroy an actor's thread."
  (remhash (actor-name actor) *registry*)
  (bt2:destroy-thread (actor-thread actor)))

(defun close-and-join-actors (&rest actors)
  (mapc #'close-actor actors)
  (mapc #'join-actor actors))

(defun send (actor &rest args &aux (actor (resolve-actor actor)))
  "Asyncronously send a message to an actor."
  (send-signal actor (make-async-signal :msg args)))

(defun ask (actor &rest args &aux (actor (resolve-actor actor)))
  "Syncronously send a message and await a response from an actor"
  (let ((sem (bt2:make-semaphore)))
    (send-signal actor (make-sync-signal :msg args :sem sem))
    (bt2:wait-on-semaphore sem)
    (apply #'values (actor-store actor))))

(defmacro define-actor (name state args &body body)
  "This macro creates a function named `name' that spawns an instance of
the actor.  `state' is a let-list of mutable data owned by the
actor.  `args' is a lambda-list to destructure messages with, and `body'
is the code to handle messages."
  (let ((behav (intern (string-upcase (format nil "~a-behav" name)))))
    `(progn
       (defun ,behav ,args
         ,@(mapcar (lambda (pair) `(declare (special ,(car pair)))) state)
         ,@body)
       (defun ,name (&key name)
         (when (gethash name *registry*)
           (error "Actor named ~a already exists." name))
         (let ((actor (make-actor :behav ',behav :name name)))
           (setf (actor-thread actor)
                 (bt2:make-thread
                  (lambda ()
                    (let ,state
                      ,@(mapcar (lambda (pair) `(declare (special ,(car pair)))) state)
                      (run-actor actor)))))
           (if name
               (setf (gethash name *registry*) actor)
               actor))))))
