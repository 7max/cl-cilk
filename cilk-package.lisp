(cl:defpackage :cilk
  (:use :asdf 
        :closer-common-lisp 
        :closer-mop
	:metabang-bind
	:cl-log
        :arnesi
        :demacs
        :iterate
        :bordeaux-threads
        ;; :anaphora
	)
  (:shadowing-import-from :cl-log :get-logger)
  (:shadowing-import-from :arnesi :else :ensure-list))



