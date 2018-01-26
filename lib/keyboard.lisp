;;;; keyboard.lisp
;;;; Platform independent keyboard library interfaces, the
;;;; implementation of these interface is platform specific.

(in-package :cl-user)
(defpackage :cl-keyboard/lib
  (:use :cl))
(in-package :cl-keyboard/lib)

(defclass <key-event> ()
  ((key :accessor key-event-key)
   (kind :accessor key-event-kind)))

(defclass <global-event-handler> ()
  ((func :accessor event-handler-function)))
(defclass <global-event-table-handler> (<global-event-handler>)
  ((table :accessor event-handler-table)))

(defgeneric key-event-loop (event-handler))
(defgeneric send-key-event ())

#+windows
(load #P"windows.lisp")
#+linux
(load #P"linux.lisp")
#+darwin
(load #P "darwin.lisp")
