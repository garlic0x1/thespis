(defpackage #:thespis
  (:use #:cl)
  (:export
   ;; thespis.lisp
   #:actor                 ; STRUCT
   #:actor-p               ; PREDICATE
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
   #:actor-fail            ; ACCESSOR
   #:actor-queue           ; ACCESSOR
   #:actor-sem             ; ACCESSOR
   #:actor-thread          ; ACCESSOR
   #:actor-openp           ; ACCESSOR
   #:actor-store           ; ACCESSOR
   ))
