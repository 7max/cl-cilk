(cl:in-package :cilk)
(PROGN
  (DEFUN _FIB-SLOW-CLONE (WORKER54 TASK55)
    (declare (type worker worker54)
             (type task task55))
    (SYMBOL-MACROLET ((N262 (the fixnum (AREF TASK55 15)))
                      (N61 (the fixnum (AREF TASK55 14)))
                      (BLOCK-RESULT58 (AREF TASK55 13))
                      (N (THE FIXNUM (AREF TASK55 12)))
                      (PC56 (THE SYMBOL (AREF TASK55 11)))
                      (OUTER-TAGBODY-RECEIVER53 (AREF TASK55 10)))
      (BLOCK |outer-block63|
        (TAGBODY
           (CASE PC56
             (:AFTER-SYNC-2 (GO :AFTER-SYNC-2))
             (:AFTER-SPAWN-1 (GO :AFTER-SPAWN-1))
             (:AFTER-SPAWN-0 (GO :AFTER-SPAWN-0)))
           (IF (NOT (< N 2))
               (GO IFELSE59) NIL)
           (SETQ BLOCK-RESULT58 N)
           (GO IFEND60)
         IFELSE59
           (SETQ PC56 ':AFTER-SPAWN-0)
           (SETQ N61 (_FIB-FAST-CLONE WORKER54 TASK55 14 (the fixnum (- N 2))))
           (POP-FRAME-CHECK WORKER54 TASK55)
         :AFTER-SPAWN-0
           (SETQ PC56 ':AFTER-SPAWN-1)
           (SETQ N262 (_FIB-FAST-CLONE WORKER54 TASK55 15 (the fixnum (- N 1))))
           (POP-FRAME-CHECK WORKER54 TASK55)
         :AFTER-SPAWN-1
           (SETQ PC56 ':AFTER-SYNC-2)
           (SYNC-CHECK WORKER54 TASK55)
         :AFTER-SYNC-2
           (SETQ BLOCK-RESULT58
                 (THE FIXNUM (+ (THE FIXNUM N61) (THE FIXNUM N262))))
         IFEND60
         AFTER-BLOCK57
           (SETQ OUTER-TAGBODY-RECEIVER53 BLOCK-RESULT58)
           (RETURN-FROM |outer-block63| OUTER-TAGBODY-RECEIVER53)))))
  (DEFUN _FIB-FAST-CLONE (WORKER54 |parent64| |parent-spawn-num65| N)
    (DECLARE (TYPE WORKER WORKER54) (TYPE SIMPLE-VECTOR |parent64|)
             (TYPE FIXNUM |parent-spawn-num65|)
             (type fixnum n))
    (LET ((TASK55
           (ALLOC-TASK WORKER54 16 |parent64| |parent-spawn-num65|
                       #'_FIB-SLOW-CLONE)))
      (DECLARE (TYPE SIMPLE-VECTOR TASK55))
      NIL
      (ATOMIC-ADD (TASK-NUM-CHILDREN |parent64|) 1)
      (SETF (TASK-CHILD |parent64|) TASK55)
      (UNLESS (WORKER-FIRST-TASK WORKER54)
        (SETF (WORKER-FIRST-TASK WORKER54) TASK55))
      (SETF (TASK-STATE |parent64|) (WORKER-READY-STATE WORKER54))
      (SETF (THE FIXNUM (AREF TASK55 12)) N)
      (SYMBOL-MACROLET ((N262 (the fixnum (AREF TASK55 15)))
                        (N61 (the fixnum (AREF TASK55 14)))
                        (BLOCK-RESULT58 (AREF TASK55 13))
                        (N (THE FIXNUM (AREF TASK55 12)))
                        (PC56 (THE SYMBOL (AREF TASK55 11)))
                        (OUTER-TAGBODY-RECEIVER53 (AREF TASK55 10)))
        (BLOCK |outer-block63|
          (TAGBODY
             (IF (NOT (< N 2))
                 (GO IFELSE59) NIL)
             (SETQ BLOCK-RESULT58 N)
             (GO IFEND60)
           IFELSE59
             (SETQ PC56 ':AFTER-SPAWN-0)
             (SETQ N61 (_FIB-FAST-CLONE WORKER54 TASK55 14 (- N 2)))
             (POP-FRAME-CHECK WORKER54 TASK55)
           :AFTER-SPAWN-0
             (SETQ PC56 ':AFTER-SPAWN-1)
             (SETQ N262 (_FIB-FAST-CLONE WORKER54 TASK55 15 (- N 1)))
             (POP-FRAME-CHECK WORKER54 TASK55)
           :AFTER-SPAWN-1
           :AFTER-SYNC-2
             (SETQ BLOCK-RESULT58
                   (THE FIXNUM (+ (THE FIXNUM N61) (THE FIXNUM N262))))
           IFEND60
           AFTER-BLOCK57
             (SETQ OUTER-TAGBODY-RECEIVER53 BLOCK-RESULT58)
             (RETURN-FROM |outer-block63| OUTER-TAGBODY-RECEIVER53))))))
  (DEFUN FIB (N)
    (IF *WORKER*
        (DO-WORKER *WORKER*
          (LAMBDA (WORKER)
            (_FIB-FAST-CLONE WORKER (CREATE-ROOT-TASK WORKER)
                             FIRST-TASK-RESULT N)))
        (START-WORKER
         (LAMBDA (WORKER)
           (_FIB-FAST-CLONE WORKER (CREATE-ROOT-TASK WORKER) FIRST-TASK-RESULT
                            N))))))



