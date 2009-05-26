;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-


(asdf::defsystem :cilk
  :serial t
  :depends-on (:cl-log :arnesi :demacs :iterate)
  :components (
               (:file "cilk-package")
               (:file "catch-case")
               (:file "cilk-runner")
               (:file "cilk")))

