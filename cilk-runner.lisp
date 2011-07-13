(cl:in-package :cilk)

(macrolet ((def-task-field (name index) 
             (let ((accessor (intern (format nil "TASK-~a" name))))
               (with-unique-names (obj)
                 `(def macro ,accessor (,obj)
                    `(svref (the simple-vector ,,obj) 
                            ,',index))))))
  (def-task-field state 0) 
  (def-task-field children 1) 
  (def-task-field parent-spawn-num 2) 
  (def-task-field parent 3)
  (def-task-field slow-clone 4) 
  (def-task-field lock 5) 
  (def-task-field name 6)
  ;; for non-root tasks after the first task-results are the results
  ;; of more spawn statements and then closed over variables
  (defconstant first-task-result 7)
  ;; for root task after the 1st result there is done flag
  (def-task-field done 8)
  (defconstant root-task-size (+ 2 first-task-result)))

(deftype task () 'simple-vector)

(def function lisp-obj-addr (obj)
  (sb-kernel::get-lisp-obj-address obj))

(defun task-desc (task)
  (aif (task-parent task)
       (format nil "<task ~s ~x parent ~s state ~s pc ~s>" 
               (task-name task) 
               (lisp-obj-addr task)
               (task-name (task-parent task)) (task-state task)
               (aref task (+ first-task-result 2)))
       (format nil "<task ~s ~x state ~s pc ~s>" 
               (task-name task) 
               (lisp-obj-addr task)
               (task-state task)
               (aref task (+ first-task-result 2)))))

(def macro my-compare-and-swap (place old new)
  (let ((place (macroexpand place)))
    `(sb-ext:compare-and-swap ,place ,old ,new)))

(defconstant max-depth 16384)

(def struct (worker (:constructor make-worker-1)
                    (:print-function 
                     (lambda (obj stream n)
                       (declare (ignore n))
                       (print-unreadable-object (obj stream :type t)
                         (format stream "~d head ~d/~d/~d" 
                                 (worker-worker-num obj)
                                 (worker-runqueue-except obj)
                                 (worker-runqueue-head obj) 
                                 (worker-runqueue-tail obj))))))
  (initial-job-done nil)
  (initial-job-result nil)
  (worker-num nil :type fixnum)
  (running-state nil :type symbol)
  (ready-state nil :type symbol)
  (thread nil)
  (tasks (make-array max-depth :initial-element nil)
         :type simple-vector)
  (runqueue-head 0 :type fixnum)
  (runqueue-tail -1 :type fixnum)
  (runqueue-except 0 :type fixnum)
  (lock (make-lock)))


(def (function i) assert-runqueue-valid (worker)
  (declare (type worker worker))
  (declare (ignorable worker))
  ;; (assert (and (< (worker-runqueue-tail worker) max-depth)
  ;;              (<= (1- (worker-runqueue-head worker))
  ;;                  (worker-runqueue-tail worker))))
  )

(def function runqueue-empty-p (worker)
  (declare (type worker worker))
  (assert-runqueue-valid worker)
  (< (worker-runqueue-tail worker)
     (worker-runqueue-head worker)))

(def function runqueue-size (worker)
  (declare (type worker worker))
  (assert-runqueue-valid worker)
  (1+ (- (worker-runqueue-tail worker)
         (worker-runqueue-head worker))))

(def macro with-worker-lock ((worker) &body body)
  "Evaluate BODY while holding worker lock"
  `(with-lock-held ((worker-lock ,worker))
     ,@body))

(def macro with-task-lock ((task) &body body)
  "Evaluate BODY while holding task lock"
  `(with-lock-held ((task-lock ,task))
     ,@body))

(def (function i) alloc-task (worker task-size parent parent-spawn-num slow-clone)
  (declare (type fixnum task-size parent-spawn-num)
           (type worker worker)
           (type (or null task) parent)
           (type (or null function) slow-clone))
  ;; (assert (>= parent-spawn-num first-task-result))
  ;; (assert-runqueue-valid worker)
  (let ((h (1+ (worker-runqueue-tail worker))))
    (declare (fixnum h))
    (let ((task (aref (worker-tasks worker) h)))
      (declare (type (or null task) task))
      (cond ((and task (>= (length task) task-size))
             ;; (assert (member (task-state task) 
             ;;                 '(:done-fast :done-slow)))
             )
            (t
             (setf (aref (worker-tasks worker) h)
                   (setf task
                         (make-array task-size)))))
      (setf (task-parent task) parent)
      (setf (task-parent-spawn-num task) parent-spawn-num)
      (setf (task-slow-clone task) slow-clone)
      (setf (task-children task) nil)
      (setf (task-lock task) nil)
      (setf (task-state task) (worker-running-state worker))
      task)))

(def function make-worker (&rest args)
  (let ((worker (apply #'make-worker-1 args)))
    (setf (worker-running-state worker)
          (intern (format nil "RUNNING_~d" (worker-worker-num worker)) :keyword)
          (worker-ready-state worker)
          (intern (format nil "READY_~d" (worker-worker-num worker)) :keyword))
    worker))


(defconstant max-workers 16)
;; defvar instead of def var so it wont get overwritten 
;; when reloading the file
(defvar *workers* (make-array max-workers :initial-element nil))
(defvar *num-workers* 0)
(declaim (type fixnum *num-workers*))
(def special-variable *workers-flag* nil)
(def special-variable *worker* nil)
(defvar *initial-runqueue-tail*)

(def function kill-workers ()
  "Kill all the worker threads"
  (mapc #'sb-thread:terminate-thread 
        (map 'list #'worker-thread (remove nil *workers*))))

(defconstant tag-worker-is-free 'tag-worker-is-free)
(defconstant tag-worker-run-task 'tag-worker-run-task)

(def macro atomic-replace (place newvalue)
  (let ((new (gensym)))
    `(loop 
       as ,new = ,newvalue
       until (eq ,new (my-compare-and-swap ,place ,place ,new)))))

(def macro atomic-add (place diff)
  (let ((new (gensym))
        (old (gensym))
        (d (gensym)))
    `(loop 
        with ,d fixnum = ,diff
        as ,old fixnum = (the fixnum ,place)
        as ,new fixnum = (the fixnum (+ ,old ,d))
        until (= ,old (the fixnum (my-compare-and-swap ,place ,old ,new)))
        finally (return ,old))))

(defmacro error* (&rest arguments)
  (let ((msg (gensym)))
    `(let ((,msg (format nil ,@arguments)))
       (log-error "~a" ,msg)
       (error ,msg))))

(def special-variable *worker-bindings*
  `((*standard-output* ,*standard-output*)
    (*print-circle* t)
    (*terminal-io* ,*terminal-io*)
    (*debug-io* ,*debug-io*)))

(def function do-recursive-worker (worker func)
  "Caled when a cilk procedure is caled without a spawn, and we
already running another cilk procedure "
  (declare (type function func))
  (let* (task parent saved-tail)
    (with-worker-lock (worker)
      (setf saved-tail (worker-runqueue-tail worker))
      (setf task (aref (worker-tasks worker) saved-tail))
      (setf parent (task-parent task))
      (assert (<= (worker-runqueue-head worker) 
                  (worker-runqueue-tail worker)))
      ;; (assert (not (member (task-state task) 
      ;;                      '(:done :done-fast :rooted :rooted2 :rooted3))))
      ;; (assert (eq (task-state task) 
      ;;             (worker-running-state worker)))
      (cond ((= (worker-runqueue-head worker) saved-tail)
             ;; we are topmost running task, everything below
             ;; including our parent is stolen, or we are 
             ;; the very first task. In either case head should
             ;; advance above us
             (incf (worker-runqueue-head worker))
             (incf (worker-runqueue-except worker)))
            (t 
             ;; otherwise mark this task as :rooted so
             ;; steal-task will jump over us
             (setf (task-state task) :rooted)))
      ;; (cond ((and 
      ;;         (> (worker-runqueue-tail worker) 
      ;;            (1+ *initial-runqueue-tail*))
      ;;         ;; they are all stolen
      ;;         (= (worker-runqueue-head worker) saved-tail))
      ;;        ;; then our parent is already stolen
      ;;        (assert (task-lock parent))
      ;;        (assert (member task (task-children parent)))
      ;;        (setf (task-state task) :rooted2)
      ;;        (setf reset-head t))
      ;;       ( ;; we are first task on this CPU
      ;;        (= (worker-runqueue-head worker) saved-tail)
      ;;        ;; means our parent is root task, or we are
      ;;        ;; stolen and our parent is on diff CPU
      ;;        (assert (or (null (task-parent parent))
      ;;                    (member task (task-children parent))))
      ;;        (setf (task-state task) :rooted3)
      ;;        (setf reset-head t))
      ;;       (t
      ;;        ;; else our parent is notyet stolen
      ;;        (assert
      ;;         (or (eq (worker-ready-state worker) 
      ;;                 (task-state parent))
      ;;             (null (task-parent parent))))
      ;;        ;; then our parent is not stolen yet
      ;;        (setf (task-state task)  :rooted)
      ;;        (assert (< (worker-runqueue-head worker) saved-tail))))
      )
    (let* ((result
            (do-worker worker func)))
      ;; (log-debug "~s noncilk->cilk parent = ~s"
      ;;            worker 
      ;;            (task-desc task))
      (with-worker-lock (worker) 
        ;; (assert-runqueue-valid worker)
        ;; (assert (= (worker-runqueue-tail worker) saved-tail))
        ;; can't point to us, either we moved it up
        ;; or steal-task should have
        ;; (assert (/= (worker-runqueue-head worker) saved-tail))
        ;; (assert (eq (aref (worker-tasks worker) saved-tail) 
        ;;             task))
        (cond ((> (worker-runqueue-head worker) saved-tail)
               ;; (assert (member (task-state task) 
               ;;                 '(:rooted :rooted2 :rooted3)))
               (setf (worker-runqueue-head worker) saved-tail)
               (setf (worker-runqueue-except worker) saved-tail))
              (t
               ;; never went above us
               ;; (assert (member (task-state task) 
               ;;                 '(:rooted)))
               ;; (assert (not reset-head))
               ))
        (setf (task-state task) (worker-running-state worker)))
      ;; (log-debug "~s noncilk->cilk ~s returning ~s" 
      ;;            worker
      ;;            (list ',name ,@args)
      ;;            result)
      ;; (log-debug "~s tasks ~s" worker 
      ;;            (map 'list #l (and !1 (task-desc !1)) 
      ;;                 (subseq (worker-tasks worker) 0 10)))
      result)))

(def function start-worker (&optional current-thread-task)
  (let ((worker (make-worker  
                 :worker-num (atomic-add (symbol-value '*num-workers*) 1))))
    (let 
        ((func 
          (lambda ()
            (let (result)
              (progv
                  (mapcar #'first *worker-bindings*)
                  (mapcar #'second *worker-bindings*)
                (unwind-protect 
                     (let ((*worker* worker))
                       (setf (worker-thread worker) sb-thread:*current-thread*)
                       ;; atomically register ourselfs in the *workers* array
                       (unless (null (my-compare-and-swap 
                                      (svref *workers* (worker-worker-num worker))
                                      nil worker))
                         (error* "The *workers* slot ~d was not nil"
                                 (worker-worker-num worker)))
                       (log-debug "~d started" worker)
                       (setq result (do-worker worker current-thread-task)))
                  (atomic-add (symbol-value '*num-workers*) -1)
                  ;; atomically unregister ourselfs from the *workers* array
                  (unless (eq worker (my-compare-and-swap 
                                      (svref *workers* (worker-worker-num worker))
                                      worker nil))
                    (log-error "The *workers* slot ~d was not equal to myself"
                               (worker-worker-num worker)))
                  (log-debug "~d terminated" worker)))
              result))))
      (if current-thread-task
          (funcall func)
          (prog1 nil (sb-thread:make-thread func))))))

(def function do-worker (worker initial-lambda)
  (let ((*initial-runqueue-tail* (worker-runqueue-tail worker)))
    (if initial-lambda
        (let ((root-task (create-root-task)))
          (flet ((initial-task (worker)
                   (setf (aref root-task first-task-result) 
                         (funcall initial-lambda worker root-task))
                   (setf (task-done root-task) t)))
            (do-worker-1 worker nil #'initial-task))
          (loop
             (cond ((task-done root-task)
                    (return (aref root-task first-task-result)))
                   ((eq *workers-flag* :quit)
                    (return)))
             (do-worker-1 worker nil nil)))
        (loop
           (do-worker-1 worker nil nil)
           (cond ((eq *workers-flag* :quit)
                  (return)))))))

(def function do-worker-1 (worker &optional resumed-task initial-task)
  ;; initially we got nothing to steal
  ;; (with-worker-lock (worker)
  ;;   (assert (and (= (worker-runqueue-tail worker) *initial-runqueue-tail*)))
  ;;   (assert-runqueue-valid worker))
  (catch-case 
      (cond (resumed-task 
             (log-debug "~s resumed-task=~s" worker (task-desc resumed-task))
             (run-slow-task worker resumed-task))
            ;; only happens once
            (initial-task 
             (log-trace "~s initial-task=~s" worker initial-task)
             ;; if below returns, then we had completed fast cilk procedure
             ;; without anyone stealing it, if someone steals it it will
             ;; signal from iside teh funcall
             (funcall (the function initial-task) worker)
             (log-trace "~s initial task returned" worker)
             (with-worker-lock (worker)
               (assert (= (worker-runqueue-tail worker)
                          (+ 1 *initial-runqueue-tail*)))
               (decf (worker-runqueue-tail worker)
                     1)))
            (t (find-and-do-some-work worker)))
    (tag-worker-is-free
     (log-debug "~s free" worker))
    (tag-worker-run-task
     (log-debug "~s run-task ~s" worker (task-desc it))
     (do-worker-1 worker it))
    (t
     ;; (log-error "~d unexpected return" (worker-worker-num worker))
     )))


(def function create-root-task ()
  (let ((task (make-array root-task-size)))
    (setf (task-parent-spawn-num task) first-task-result)
    (setf (task-parent task) nil)
    (setf (task-lock task) (make-lock))
    (setf (task-children task) nil)
    (setf (task-name task) "root")
    (setf (task-done task) nil)
    task))

(def function run-slow-task (worker task)
  "Run the slow clone (after a sync point or steal"
  (log-debug "~s resuming ~s" worker (task-desc task))
  (with-task-lock (task)
    ;; (assert-runqueue-valid worker)
    (setf (task-state task)
          (worker-running-state worker)))
  ;; put into steal list
  (with-worker-lock (worker)
    ;; We need to set number of children to zero because
    ;; sync protocol leaves it as -1 after the sync
    ;; (setf (task-num-children task) 0)
    ;; (assert (null (find task (worker-tasks worker))))
    ;; put into runqueue
    (setf (aref (worker-tasks worker)
                (incf (worker-runqueue-tail worker)))
          task))
  ;; finally run it
  (task-returned worker task 
                 (funcall (the function (task-slow-clone task)) worker task)))

(def function find-and-do-some-work (worker)
  (acond 
         ((steal-task worker)
          (run-slow-task worker it))
         (t 
          (sleep 0.001)
          nil)))

(def function task-returned (worker task result)
  "Handle once stolen cilk procedure doing a normal return"
  (log-debug "~s task ~s result ~s" 
             worker
             (task-desc task) result)
  (acond ((task-parent task)
          ;; we have a parent, forward result to them
          (assert (>= (the fixnum (task-parent-spawn-num task)) first-task-result))
          (setf (aref (the simple-vector it) (task-parent-spawn-num task)) result)
          (with-worker-lock (worker)
            (child-returned worker it task)))
         (t (log-info "~s Initial task returned ~s" worker result))))

(def function clear-runqueue (worker)
  "Reset worker runqueue tail / head to initial position, thus
clearing it"
  (setf (worker-runqueue-tail worker) *initial-runqueue-tail*)
  (setf (worker-runqueue-except worker) (1+ (worker-runqueue-tail worker)))
  (setf (worker-runqueue-head worker) (1+ (worker-runqueue-tail worker))))

(def function signal-resume-task (worker task)
  "Signal the current worker to resume specified task"
  (clear-runqueue worker)
  (throw tag-worker-run-task task))

(def function signal-worker-free (worker)
  "Signal the current worker that its now free"
  (clear-runqueue worker)
  (throw tag-worker-is-free nil))


(def function set-initial-worker-returned (root-task)
  "Set the flag in the initial worker that the initial task had
returned"
  (let ((result (aref root-task first-task-result)))
    (log-debug "last-child of initial-task returned result = ~s" result)
    (setf (task-done root-task) t)))


(def function child-returned (worker parent child)
  "Called for when parent was stolen or is on a different CPU and
the spawned child had returned. At the point of the call teh
result of the spawned child is already stored in the target
of (setf result (spawn statemenet)).

Atomically decrements the children count of the parent, and if
this was a last child resumes the parent on this CPU, otherwise
signals that this worker is free.

Must be called with the worker lock held"
  (declare (type worker worker)
           (type task parent child)
           (ignorable worker))
  ;; (assert-runqueue-valid worker)
  (with-task-lock (parent)
    ;; (assert (or (null (task-parent parent)) 
    ;;             (member child (task-children parent))))
    (setf (task-children parent)
          (delete child (task-children parent)))
    (if (null (task-children parent))
        (if (null (task-parent parent)) 
            (set-initial-worker-returned parent)
            (if (eq (task-state parent) :waiting)
                (signal-resume-task worker parent))))
    (signal-worker-free worker)))

#+cilk-fence
(def function steal-task (worker)
  (when-let (victim (choose-victim worker))
    (assert (not (eq victim worker)))
    (symbol-macrolet
        ((E (worker-runqueue-except victim))
         (H (worker-runqueue-head victim))
         (Ta (worker-runqueue-tail victim)))
      (with-worker-lock (victim)
        (incf E)
        (mfence)
        (if (< H Ta)
            ;; sucessful steal
            (let ((task (aref (worker-tasks victim) H)))
              (log-debug "~s stolen from ~s task ~s" worker
                       victim (task-desc task))
              
              (setf (task-state task) :stolen)
              ;; steal was ok, promote the task by creating lock at
              ;; this point the pop-frame-check-failed path will be
              ;; entered by the child upon return, and it will block
              ;; at worker lock
              (unless (task-lock task)
                (assert (null (task-children task)))
                (setf (task-lock task) (make-lock)))
              ;; since we are moving task to a diff stack
              ;; can't reuse it
              (setf (aref (worker-tasks victim) 
                          (worker-runqueue-head victim)) 
                    nil)
              (incf H)
              ;; cpu executing our child will get blocked in
              ;; pop-frame-check-falied, therefore its safe to
              ;; manipulate parent without locking
              (let ((child (aref (worker-tasks victim)
                                 (worker-runqueue-head victim))))
                (add-task-child task child)
                (iterate 
                  (while (eq (task-state child) :rooted))
                  (incf (worker-runqueue-head victim))
                  (incf (worker-runqueue-except victim))
                  (log-debug "Rooted child, victim is now ~s" victim)
                  (setf child (aref (worker-tasks victim) 
                                    (worker-runqueue-head victim)))
                  (while child)))
              task)
            ;; failed steal, retract the exception
            (prog1 nil (decf E)))))))

#+cilk-status
(def function steal-task (worker)
  (when-let (victim (choose-victim worker))
    (assert (not (eq victim worker)))
    (with-worker-lock (victim)
        (let ((task (aref (worker-tasks victim)
                          (worker-runqueue-head victim))))
          ;; do actual steal
          (when (and task
                     (eq (worker-ready-state victim)
                         (my-compare-and-swap (task-state task)
                                              (worker-ready-state victim)
                                              (worker-running-state worker))))
            (log-debug "~s stolen from ~s task ~s" worker
                       victim (task-desc task))
            ;; steal was ok, promote the task by creating lock
            ;; at this point the pop-frame-check-failed path will be entered by the child
            ;; upon return, and it will block at worker lock
            (unless (task-lock task)
              (assert (null (task-children task)))
              (setf (task-lock task) (make-lock)))
            ;; since we are moving task to a diff stack
            ;; can't reuse it
            (setf (aref (worker-tasks victim) 
                        (worker-runqueue-head victim)) 
                  nil)
            (incf (worker-runqueue-head victim))
            (incf (worker-runqueue-except victim))
            ;; cpu executing our child will get blocked in
            ;; pop-frame-check-falied, therefore its safe to
            ;; manipulate parent without lockingit
            (let ((child (aref (worker-tasks victim)
                               (worker-runqueue-head victim))))
              ;; rooted child means that it called some non-cilk
              ;; code which recursively called (defcilk) procedure
              ;; in this case we can can not steal the next task
              ;; as it has a non-cilk code in its stack, waiting
              ;; for cilk procedure it called to return..
              ;;
              ;; So we skip over such task, but allow steals 
              ;; for any children of it
              ;;
              ;; Normal case, arrow indicates newhead:
              ;;   [-- fast grandchild ]
              ;; > [-- fast child ]
              ;;   [ victim task being stolen ]
              ;;
              ;; Rooted case:
              ;;
              ;;   [--fast grandchild]
              ;; > [--fast child--] 
              ;;   *[-- new root cilk task, new (do-worker) loop --]
              ;;   *[--non-cilk frame calling into cilk -= ]
              ;;   [-- fast child :rooted ]
              ;;   [ victim task being stolen ]
              ;; 
              ;;  * tasks are on CPU stack, but not on the cilk tasks stack
              (add-task-child task child)
              (when (eq (task-state child) :rooted)
                (incf (worker-runqueue-head victim))
                (incf (worker-runqueue-except victim)))
              (iterate
                (while (and (< (worker-runqueue-head victim) 
                               (worker-runqueue-tail victim))
                            (eq (task-state 
                                 (aref (worker-tasks victim) 
                                       (worker-runqueue-head victim)))
                                :rooted)))
                (incf (worker-runqueue-head victim))
                (incf (worker-runqueue-except victim)))
              ;; (assert-runqueue-valid worker)
              ;; (assert-runqueue-valid victim)
              )
            task)))))

(def function add-task-child (task child)
  (declare (type task task child))
  (with-task-lock (task)
    (log-debug "~s parent ~s child ~s" 
               *worker*
               (task-desc task)
               (task-desc child))
    (assert (not (eq task child)))
    (assert (null (member child (task-children task))))
    (push child (task-children task))))

(def function choose-victim (worker)
  "Randomly choose a victim among other workers"
  (when (> *num-workers* 1)
    (let ((range  (1- (the (integer 0 16) *num-workers*))))
      (when (plusp range)
        (let* ((n (random range)))
          (declare (type (integer 0 16) range))
          (when (>= n (worker-worker-num worker))
            (incf n))
          (when (and (< n *num-workers*)
                     (not (= n (worker-worker-num worker))))
            (svref *workers* n)))))))

(def function pop-frame-check-failed (worker task)
  "Handles the case where task was stolen"
  (declare (type worker worker)
           (type task task))
  ;; (assert-runqueue-valid worker)
  (with-worker-lock (worker)
    (log-debug "~s task ~s was stolen by someone" 
               worker (task-desc task))
    ;; anything below us as stolen, need 1+ because pop-frame-check
    ;; decrements the tail
    ;; no need to, steal-task now nulls it
    ;; (fill (worker-tasks worker) nil :end (1+ (worker-runqueue-tail worker)))
    ;; parent was stolen, so do the child-returned while parent
    ;; is on diff CPU protocol
    (child-returned worker task 
                    (aref (worker-tasks worker)
                          (worker-runqueue-head worker)))))

#+cilk-fence
(def (function) pop-frame-check (worker task)
  "Called after every spawn had returned"
  (declare (type worker worker)
           (type task task))
  (symbol-macrolet 
      ((Ta (worker-runqueue-tail worker))
       (E (worker-runqueue-except worker))
       (H (worker-runqueue-head worker)))
    (decf Ta)
    (mfence)
    (unless (<= E Ta)                   ;stolen??
      (pop-frame-check-failed worker task))
    (assert (not (eq (task-state task) :stolen)))
    (assert (eq task (aref (worker-tasks worker) 
                           Ta)))))

#+cilk-status
(def (function i) pop-frame-check (worker task)
  "Called after every spawn had returned"
  (declare (type worker worker)
           (type task task))
  ;; (assert-runqueue-valid worker)
  (cond ((eq (worker-ready-state worker)
             (my-compare-and-swap (task-state task)
                                  (worker-ready-state worker)
                                  (worker-running-state worker)))
         ;; successfully swapped the state from ready_CPU to
         ;; running_CPU therefore we are the only child, so safe to
         ;; just decrement child count and return
         ;; (log-debug "~d task ~s not stolen" (worker-worker-num worker) (task-desc task))
         ;; (assert (plusp (the fixnum (task-num-children task))))

         ;; we are called from fast clone, which was not stolen, therefore its
         ;; safe te simply decreament here
         ;; (atomic-add (task-num-children task) -1)
         (decf (worker-runqueue-tail worker)))
        (t
         (decf (worker-runqueue-tail worker))
         (pop-frame-check-failed worker task))))

(def function sync-check (worker task)
  ;; a running slow task should be the only task in this CPU runqueue
  (with-task-lock (task)
    ;; (assert-runqueue-valid worker)
    (log-debug "~s task=~s length of child list = ~d" 
               worker
               (task-desc task)
               (length (task-children task)))
    (when (not (null (task-children task)))
      ;; (assert-runqueue-valid worker)
      (setf (task-state task) :waiting)
      ;; waiting task will be resumed on a diff CPU by the last
      ;; returning child. Therefore we need to remove it from
      ;; the current stack, so that its task record is not reused
      ;; ba alloc-task on this CPU
      ;; (assert (eq task (aref (worker-tasks worker)
      ;;                        (worker-runqueue-tail worker))))
      (setf (aref (worker-tasks worker) 
                  (worker-runqueue-tail worker))
            nil)
      (signal-worker-free worker))))

