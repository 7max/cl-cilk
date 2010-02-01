;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-


(asdf::defsystem :cilk
  :serial t
  :depends-on (:closer-mop :demacs :cl-log :iterate :alexandria :stefil :bordeaux-threads :metabang-bind :arnesi :cl-maxlib)
  :components (
               (:file "cilk-package")
               (:file "catch-case")
               (:file "memory-barrier")
               (:file "cilk-runner")
               (:file "cilk")
               (:file "tests")))


