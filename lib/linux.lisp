(in-package :cl-keyboard/lib)
(named-readtables:in-readtable lol:lol-syntax)

(defun key-event-loop (event-handler)
  )

(defun send-key-event (event)
  )


(defun %get-uinput-devices ()
  (let ((devices
         (handler-case (alexandria:read-file-into-string #P"/proc/bus/input/devices")
           (file-error () (return-from %get-uinput-devices nil)))))
    (ppcre:register-groups-bind (name handlers)
        ((ppcre:create-scanner "N: Name=\\\"([^\"]+?)\".+?H: Handlers=([^\\n]+)" :multi-line-mode t) devices)
      (print name)
      (print handlers))))


(named-readtables:in-readtable :standard)
