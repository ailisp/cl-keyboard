;;;; cl-keyboard.lisp
(asdf:load-system :trivial-features)

(asdf:defsystem #:cl-keyboard
  :description "Capture and customize keyboard behavior"
  :author "Bo Yao <icerove@gmail.com>"
  :serial t
  :components ((:module "lib"
                        :components ((:file "keyboard")
                                     (:file "windows" :if-feature :windows)
                                     (:file "linux" :if-feature :linux)
                                     (:file "mac" :if-feature :darwin))))
  :depends-on (:trivial-features
               :cffi
               :bordeaux-threads
               :alexandria
               :cl-ppcre
               :let-over-lambda))
