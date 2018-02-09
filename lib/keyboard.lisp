;;;; keyboard.lisp
;;;; Platform independent keyboard library interfaces, the
;;;; implementation of these interface is platform specific.
;;;; Each platform should implement at least two functions:
;;;;
;;;; KEY-EVENT-LOOP (EVENT-HANDLER)
;;;; An event loop running infinitely, and whenever receives
;;;; a new event, will call (EVENT-HANDLER event). Should be
;;;; call in a separate thread if you don't want to block
;;;; main thread. EVENT-HANDLER should take the event as
;;;; argument and return T if event is processed, return NIL
;;;; to pass the event to system default event handler.
;;;;
;;;; SEND-KEY-EVENT (EVENT)
;;;; Send a event given by EVENT, a helper function to replace
;;;; and send event or series of event in KEY-EVENT-LOOP.

(in-package :cl-user)
(defpackage :cl-keyboard/lib
  (:use :cl)
  (:export :key-event-loop
           :send-key-event
           :key-event-key
           :key-event-kind
           :key-event-time
           :key-event-source
           :make-key-event))

(in-package :cl-keyboard/lib)

(defstruct key-event
  key kind time source)

#+windows
(load #P"windows.lisp")
#+linux
(load #P"linux.lisp")
#+darwin
(load #P "darwin.lisp")
