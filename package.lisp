(defpackage #:thespis
  (:use #:cl)
  (:export
   ;; thespis.lisp
   #:actor                 ; STRUCT
   #:*self*                ; SPECIAL VARIABLE
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
