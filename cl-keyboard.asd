;;;; cl-keyboard.lisp

(asdf:defsystem #:cl-keyboard
  :description "Capture and customize keyboard behavior"
  :author "Bo Yao <icerove@gmail.com>"
  :license "BSD"
  :serial t
  :components ((:module "platform"
                        :components ((:file "platform")))
               (:file "cl-keyboard"))
  :depends-on (:trivial-features :cffi :bordeaux-threads))
