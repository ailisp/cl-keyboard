(in-package :cl-user)
(defpackage :cl-keyboard/platform
  (:use :cl))
(in-package :cl-keyboard/platform)

#+windows
(load #P"windows.lisp")

#+linux
(load #P"linux.lisp")

#+darwin
(load #P "darwin.lisp")
