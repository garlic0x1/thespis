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

(defstruct dispatcher
  (name    nil             :type (or nil symbol keyword))
  (workers nil             :type list)
  (lock    (bt2:make-lock) :type bt2:lock))

(defgeneric resolve-actor (actor)
  (:method ((actor dispatcher))
    (reduce #'min
            (dispatcher-workers actor)
            :initial-value -1
            :key (lambda (actor) (q:qsize (actor-queue actor)))))
  (:method ((actor symbol))
    (gethash actor *registry*)))

(defmethod process-message ((actor actor) msg)
  (let ((*self* actor))
    (setf (actor-store actor)
          (multiple-value-list
           (handler-case (apply (actor-behav actor) msg)
             (error (c) (funcall (actor-fail actor) c)))))))

(defmethod run-actor ((actor actor))
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

(defmethod send-signal ((actor actor) sig)
  (unless (actor-openp actor)
    (error (format nil "Message sent to closed actor: ~w" actor)))
  (q:qpush (actor-queue actor) sig)
  (bt2:signal-semaphore (actor-sem actor)))

(defgeneric close-actor (actor)
  (:documentation "Send a close-signal to an actor.")
  (:method ((actor actor))
    (send-signal actor (make-close-signal))
    (setf (actor-openp actor) nil))
  (:method ((actor t))
    (close-actor (resolve-actor actor))))

(defgeneric join-actor (actor)
  (:documentation "Wait for an actor to finish computing.")
  (:method ((actor actor))
    (bt2:join-thread (actor-thread actor))
    (apply #'values (actor-store actor)))
  (:method ((actor t))
    (join-actor (resolve-actor actor))))

(defgeneric destroy-actor (actor)
  (:documentation "Immediately destroy an actor's thread.")
  (:method ((actor actor))
    (remhash (actor-name actor) *registry*)
    (bt2:destroy-thread (actor-thread actor)))
  (:method ((actor t))
    (destroy-actor (resolve-actor actor))))

(defun close-and-join-actors (&rest actors)
  (mapc #'close-actor actors)
  (mapc #'join-actor actors))

(defgeneric send (actor &rest args)
  (:documentation "Asyncronously send a message to an actor.")
  (:method ((actor actor) &rest args)
    (send-signal actor (make-async-signal :msg args)))
  (:method ((actor t) &rest args)
    (apply #'send (cons (resolve-actor actor) args))))

(defgeneric ask (actor &rest args)
  (:documentation "Syncronously send a message and await a response from an actor")
  (:method ((actor actor) &rest args)
    (let ((sem (bt2:make-semaphore)))
      (send-signal actor (make-sync-signal :msg args :sem sem))
      (bt2:wait-on-semaphore sem)
      (apply #'values (actor-store actor))))
  (:method ((actor t) &rest args)
    (apply #'ask (cons (resolve-actor actor) args))))

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
       (defun ,name (&key name workers)
         (when (gethash name *registry*)
           (error "Actor named ~a already exists." name))
         (if workers
             (let ((dispatcher (make-dispatcher :name name)))
               (dotimes (i workers)
                 (push (,name) (dispatcher-workers dispatcher)))
               (if name
                   (setf (gethash name *registry*) dispatcher)
                   dispatcher))
             (let ((actor (make-actor :behav ',behav :name name)))
               (setf (actor-thread actor)
                     (bt2:make-thread
                      (lambda ()
                        (let ,state
                          ,@(mapcar (lambda (pair) `(declare (special ,(car pair)))) state)
                          (run-actor actor)))))
               (if name
                   (setf (gethash name *registry*) actor)
                   actor)))))))
