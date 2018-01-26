;;;; cl-keyboard.lisp
(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(cffi bordeaux-threads)))
(defpackage :cl-keyboard
  (:use :cl :cffi))
(in-package :cl-keyboard)

(define-foreign-library user32
  (:windows (:default "user32")))

(use-foreign-library user32)

(defctype HHOOK :pointer)
(defctype HOOKPROC :pointer)
(defctype HINSTANCE :pointer)
(defctype DWORD :unsigned-int)
(defcfun ("SetWindowsHookExA" set-windows-hook-ex) HHOOK
  (id-hook :int)
  (lpfn HOOKPROC)
  (h-mod HINSTANCE)
  (dw-thread-id DWORD))

(defctype LRESULT :long)
(defctype WPARAM :unsigned-int)
(defctype LPARAM :long)
(defconstant +hc-action+ 0)
(defconstant +wm-keydown+ 256)
(defconstant +wm-keyup+ 257)
(defconstant +wm-syskeydown+ 260)
(defconstant +wm-syskeyup+ 261)
(defparameter +null+ (make-pointer 0))
(defcfun ("CallNextHookEx" call-next-hook-ex) LRESULT
  (hhk HHOOK)
  (n-code :int)
  (w-param WPARAM)
  (l-param LPARAM))

(defcallback low-level-keyboard-proc LRESULT
  ((n-code :int) (w-param WPARAM) (l-param LPARAM))
  (when (eql n-code +hc-action+)
    (let ((event-processed-p
	   (cond
	     ((find w-param `(,+wm-keydown+ ,+wm-syskeydown+))
	      (let ((key-info (get-key-info l-param)))
		(if (real-key-event-p key-info)
		    (process-key-down (get-key-info l-param))
		    nil)))
	     ((find w-param `(,+wm-keyup+ ,+wm-syskeyup+))
	      (let ((key-info (get-key-info l-param)))
		(if (real-key-event-p key-info)
		    (process-key-up (get-key-info l-param))
		    nil))))))
	(if event-processed-p
	    1
	  (call-next-hook-ex +null+ n-code w-param l-param)))))

(defun real-key-event-p (key-info)
  (> (key-info-scan-code key-info) 0))

(defcstruct tag-kbdllhookstruct
  (vk-code DWORD)
  (scan-code DWORD)
  (flags DWORD)
  (time DWORD)
  (dw-extra-info DWORD))
(defctype KBDLLHOOKSTRUCT (:struct tag-kbdllhookstruct))
(defctype PKBDLLHOOKSTRUCT :pointer)
(defstruct key-info
  vk-code scan-code flags time dw-extra-info)
(defun get-key-info (ptr)
  (with-foreign-slots ((vk-code scan-code flags time dw-extra-info) (make-pointer ptr) KBDLLHOOKSTRUCT)
		      (make-key-info :vk-code vk-code
                             :scan-code scan-code
                             :flags flags
                             :time time
                             :dw-extra-info dw-extra-info)))
;;; The following two funciton, if event has been processed return t otherwise nil!
(defvar *down-keys* nil)
(defun mark-key-down (key-info)
  (pushnew key-info *down-keys* :key #'key-info-vk-code))
(defun mark-key-up (key-info)
  (setf *down-keys* (delete (key-info-vk-code key-info) *down-keys* :key #'key-info-vk-code)))
(defun key-down-p (vk-code)
  (find vk-code *down-keys* :key #'key-info-vk-code))
(defun key-up-p (vk-code)
  (not (key-down-p vk-code)))

(defconstant +vk-lshift+ #xA0)
(defconstant +vk-rshift+ #xA1)
(defconstant +vk-lcontrol+ #xA2)

(defconstant +vk-capital+ #x14)

(defconstant +vk-oem-4+ #xDB) ; in us keyboard {[ key
(defconstant +vk-oem-5+ #xDD) ; in us keyboard }] key
(defconstant +vk-9+ #x39) ; the main keyboard 9 key
(defconstant +vk-0+ #x30) ; the main keyboard 0 key

(defun process-key-down (key-info)
  (mark-key-down key-info)
  (let ((vk-code (key-info-vk-code key-info)))
    (cond
      ((eql vk-code +vk-capital+)
       (send-key-down-event +vk-lcontrol+)
       t)
      ((eql vk-code +vk-lcontrol+)
       (send-key-down-event +vk-capital+)
       t)
      ((and (eql vk-code +vk-oem-4+)
	    (key-up-p +vk-lshift+)
	    (key-up-p +vk-rshift+))
       (send-key-down-event +vk-lshift+)
       (send-key-down-event +vk-9+)
       t)
      ((and (eql vk-code +vk-oem-5+)
	    (key-up-p +vk-lshift+)
	    (key-up-p +vk-rshift+))
       (send-key-down-event +vk-lshift+)
       (send-key-down-event +vk-0+)
       t)
      ((and (eql vk-code +vk-9+)
	    (or (key-down-p +vk-lshift+)
		(key-down-p +vk-rshift+)))
       (when (key-down-p +vk-lshift+)
	 (send-key-up-event +vk-lshift+))
       (when (key-down-p +vk-rshift+)
	 (send-key-up-event +vk-rshift+))
       (send-key-down-event +vk-oem-4+)
       t)
      ((and (eql vk-code +vk-0+)
	    (or (key-down-p +vk-lshift+)
		(key-down-p +vk-rshift+)))
       (when (key-down-p +vk-lshift+)
	 (send-key-up-event +vk-lshift+))
       (when (key-down-p +vk-rshift+)
	 (send-key-up-event +vk-rshift+))
       (send-key-down-event +vk-oem-5+)
       t))))
(defun process-key-up (key-info)
  (mark-key-up key-info)
  (let ((vk-code (key-info-vk-code key-info)))
    (cond
      ((eql vk-code +vk-capital+)
       (send-key-up-event +vk-lcontrol+)
       t)
      ((eql vk-code +vk-lcontrol+)
       (send-key-up-event +vk-capital+)
       t)
      ((and (eql vk-code +vk-oem-4+)
	    (key-up-p +vk-lshift+)
	    (key-up-p +vk-rshift+))
       (send-key-up-event +vk-9+)
       (send-key-up-event +vk-lshift+)
       t)
      ((and (eql vk-code +vk-oem-5+)
	    (key-up-p +vk-lshift+)
	    (key-up-p +vk-rshift+))
       (send-key-up-event +vk-0+)
       (send-key-up-event +vk-lshift+)
       t)
      ((and (eql vk-code +vk-9+)
	    (or (key-down-p`+vk-lshift+)
		(key-down-p +vk-rshift+)))
       (send-key-up-event +vk-oem-4+)
       t)
      ((and (eql vk-code +vk-0+)
	    (or (key-down-p +vk-lshift+)
		(key-down-p +vk-rshift+)))
       (send-key-up-event +vk-oem-5+)
       t))))

(defcfun ("keybd_event" keybd-event) :void
  (b-vk :unsigned-char)
  (b-scan :unsigned-char)
  (dw-flags DWORD)
  (dw-extra-info DWORD))
(defconstant +keyenventf-keyup+ 2)

(defun send-key-down-event (vk-code)
  (keybd-event vk-code 0 0 0))
(defun send-key-up-event (vk-code)
  (keybd-event vk-code 0 +keyenventf-keyup+ 0))

(defctype HWND :pointer)
(defctype UINT :unsigned-int)
(defctype LONG :long)
(defcstruct tag-point
	(x LONG)
	(y LONG))
(defctype POINT (:struct tag-point))
(defcstruct tag-msg
	(hwnd HWND)
	(message UINT)
	(w-param WPARAM)
	(l-param LPARAM)
	(time DWORD)
	(pt POINT))

(defctype MSG (:struct tag-msg))
(defctype LPMSG :pointer)
(defctype BOOL :int)
(defcfun ("GetMessageA" get-message) BOOL
	(lp-msg LPMSG)
	(h-wnd HWND)
	(w-msg-filter-min UINT)
	(w-msg-filter-max UINT))

(defcfun ("TranslateMessage" translate-message) BOOL
	(lp-msg :pointer))

(defcfun ("DispatchMessageA" dispatch-message) LRESULT
  (lp-msg :pointer))

(defun message-loop ()
	(with-foreign-object (msg 'MSG)
		(let (result)
			(loop do (setf result (get-message msg +null+ 0 0))
				until (zerop result)
				do (progn
					(translate-message msg)
					(dispatch-message msg)))
			(foreign-slot-value msg 'MSG 'w-param))))
(defcfun ("UnhookWindowsHookEx" unhook-windows-hook-ex) BOOL
  (hhk HHOOK))

(defconstant +wh-keyboard-ll+ 13)
(defun main ()
  (let ((hhk-low-level-kbd
	 (set-windows-hook-ex +wh-keyboard-ll+ (callback low-level-keyboard-proc) +null+  0)))
    (message-loop)
    (unhook-windows-hook-ex hhk-low-level-kbd)))

(bt:make-thread #'main)
