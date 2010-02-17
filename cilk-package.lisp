(cl:defpackage :cilk
  (:use :closer-common-lisp 
        :closer-mop
	:metabang-bind
	:cl-log
        :arnesi
        :demacs
        :iterate
        :stefil
        :bordeaux-threads
        :cl-maxlib)
  #.(cons :export (let (list)
                    (when (cl:find-package :cilk)
                      (cl:do-external-symbols (s :cilk list)
                        (cl:push s list)))))
  (:import-from :alexandria :last-elt :first-elt)
  (:shadowing-import-from :cl-log :get-logger)
  (:shadowing-import-from :arnesi :else :ensure-list :eval-always))

(cl:in-package :cilk)

(eval-always
  (pushnew :cilk *features*) 
  (setq *features* (remove :cilk-fence *features*))
  (setq *features* (remove :cilk-status *features*))
  (pushnew :cilk-status *features*))


