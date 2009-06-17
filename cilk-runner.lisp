(cl:in-package :cilk)

(def macro def-task-init (task-var names)
  `(progn 
     ,@(iterate (for name in names)
                (for index from 0)
                (collect `(setf (svref ,task-var ,index) ,name)))))

(def constant max-task-results 20)

(def macro def-task-struct (struct-name &rest slots)
  (with-unique-names (obj)
    (iterate 
      (for index from 0)
      (for slot in slots)
      (for (slot-name default) = 
           (if (consp slot)
               (destructuring-bind (name &optional default) slot
                 (list name default))
               (list slot nil)))
      (for accessor = (intern (format nil "~a-~a" struct-name slot-name)))
      (collect slot-name into names)
      (collect 
          `(def macro ,accessor (,obj)
             `(svref (the simple-vector ,,obj) ,,index))
        into macros)
      (collect default into defaults)
      (finally 
       (return
         (let ((initargs 
                `(&key ,@(iterate (for name in names)
                                  (for def in defaults)
                                  (collect `(,name ,def))))))
           `(progn
              ,@macros
              (deftype task () 'simple-vector)
              (def constant first-task-result ,(length names))
              (def macro make-task (,@initargs)
                (with-unique-names (task)
                  `(let ((,task (make-array (+ max-task-results ,,(length names)))))
                     (def-task-init ,task (,,@names))
                     ,task)))
              (def macro init-task (task ,@initargs)
                `(def-task-init ,task (,,@names)))
              (def macro with-task (task (,@initargs) &body forms)
                `(let ((,task (make-array (+ max-task-results ,,(length names)))))
                   (def-task-init ,task (,,@names))
                   ,@forms)))))))))

(def-task-struct task
    (name nil)
  (state nil)
  (num-children 0)
  (children nil)
  (parent-spawn-num 0)
  (parent nil)
  (slow-clone nil)
  (lock nil)
  (initial-worker nil))


(defun task-desc (task)
  (aif (task-parent task)
       (format nil "<task ~s parent ~s state ~s pc ~s>" 
               (task-name task) (task-name (task-parent task)) (task-state task)
               (aref task (+ first-task-result 2)))
       (format nil "<task ~s  state ~s pc ~s>" 
               (task-name task) (task-state task)
               (aref task (+ first-task-result 2)))))

(def macro my-compare-and-swap (place old new)
  (let ((place (macroexpand place)))
    `(sb-ext:compare-and-swap ,place ,old ,new)))

(def constant max-depth 1024)

(def struct (worker (:constructor make-worker-1)
                    (:print-function 
                     (lambda (obj stream n)
                       (declare (ignore n))
                       (print-unreadable-object (obj stream :type t)
                         (format stream "~d" (worker-worker-num obj))))))
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
  (assert-runqueue-valid worker)
  (let ((h (incf (worker-runqueue-tail worker))))
    (declare (fixnum h))
    (let ((task (aref (worker-tasks worker) h)))
      (declare (type (or null task) task))
      (cond ((and task (>= (length task) task-size))
             task)
            (t
             (setf (aref (worker-tasks worker) h)
                   (setf task
                         (make-array task-size)))))
      (init-task task
                 :parent parent
                 :parent-spawn-num parent-spawn-num 
                 :slow-clone slow-clone)
      task)))

(def (function i) free-task (worker)
  (decf (worker-runqueue-tail worker))
  (assert-runqueue-valid worker))

(def function make-worker (&rest args)
  (let ((worker (apply #'make-worker-1 args)))
    (setf (worker-running-state worker)
          (intern (format nil "RUNNING_~d" (worker-worker-num worker)) :keyword)
          (worker-ready-state worker)
          (intern (format nil "READY_~d" (worker-worker-num worker)) :keyword))
    worker))


(def constant max-workers 16)
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

(def constant tag-worker-is-free 'tag-worker-is-free)
(def constant tag-worker-run-task 'tag-worker-run-task)

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
    (*debug-io ,*debug-io*)))

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
                       (log-info "~d started" (worker-worker-num worker))
                       (setq result (do-worker worker current-thread-task)))
                  (atomic-add (symbol-value '*num-workers*) -1)
                  ;; atomically unregister ourselfs from the *workers* array
                  (unless (eq worker (my-compare-and-swap 
                                      (svref *workers* (worker-worker-num worker))
                                      worker nil))
                    (log-error "The *workers* slot ~d was not equal to myself"
                               (worker-worker-num worker)))
                  (log-info "~d terminated" (worker-worker-num worker))))
              result))))
      (if current-thread-task
          (funcall func)
          (prog1 nil (sb-thread:make-thread func))))))

(def function do-worker (worker initial-task)
  (let ((*initial-runqueue-tail* (worker-runqueue-tail worker)))
    (loop
       (do-worker-1 worker nil initial-task)
       (cond ((eq *workers-flag* :quit)
              (return))
             ((worker-initial-job-done worker)
              (log-debug "do-worker returns because of initial-job-done")
              (return (worker-initial-job-result worker))))
       (setq initial-task nil))))


(def function do-worker-1 (worker &optional resumed-task initial-task)
  ;; initially we got nothing to steal
  (with-worker-lock (worker)
    (assert (and (= (worker-runqueue-tail worker) *initial-runqueue-tail*)))
    (assert-runqueue-valid worker))
  (catch-case 
      (cond (resumed-task (resume-slow-task worker resumed-task))
            ;; only happens once
            (initial-task 
             (setf (worker-initial-job-result worker)
                   (funcall (the function initial-task) worker)
                   (worker-initial-job-done worker) t)
             (log-debug "initial task returned")
             (with-worker-lock (worker)
               (assert (= (worker-runqueue-tail worker)
                          (+ 1 *initial-runqueue-tail*)))
               (decf (worker-runqueue-tail worker)
                     1)))
            (t (find-and-do-some-work worker)))
    (tag-worker-is-free
     (log-debug "~d free" (worker-worker-num worker)))
    (tag-worker-run-task
     (log-debug "~d run-task ~s" (worker-worker-num worker) (task-desc it))
     (do-worker-1 worker it))
    (t
     ;; (log-error "~d unexpected return" (worker-worker-num worker))
     )))


(def function create-root-task (worker)
  (let ((task (make-array (+ first-task-result 3))))
    (init-task task :parent-spawn-num first-task-result)
    ;; special magic value that child-returned funciton checks
    (setf (task-initial-worker task) worker)
    (setf (task-name task) "root")
    (setf (task-lock task) (make-lock))
    task))

(def function resume-slow-task (worker task)
  "Resume the task on this CPU after a sync point"
  (log-debug "~d resuming ~s" (worker-worker-num worker) (task-desc task))
  (with-task-lock (task)
    (assert-runqueue-valid worker)
    (assert (null (task-children task)))
    (setf (task-state task)
          (worker-running-state worker)))
  ;; put into steal list
  (with-worker-lock (worker)
    ;; We need to set number of children to zero because
    ;; sync protocol leaves it as -1 after the sync
    ;; (setf (task-num-children task) 0)
    
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
          (run-stolen-task worker it))
         (t 
          nil)))

(def function task-returned (worker task result)
  "Handle once stolen cilk procedure doing a normal return"
  (log-debug "~d task ~s result ~s" 
             (worker-worker-num worker)
             (task-desc task) result)
  (acond ((task-parent task)
          ;; we have a parent, forward result to them
          (assert (>= (the fixnum (task-parent-spawn-num task)) first-task-result))
          (setf (aref (the simple-vector it) (task-parent-spawn-num task)) result)
          (with-worker-lock (worker)
            (child-returned worker it task)))
         (t (log-info "Initial task returned ~s" result))))


(def function clear-runqueue (worker)
  "Reset worker runqueue tail / head to initial position, thus
clearing it"
  (setf (worker-runqueue-tail worker) *initial-runqueue-tail*)
  (setf (worker-runqueue-head worker) 0))

(def function signal-resume-task (worker task)
  "Signal the current worker to resume specified task"
  (clear-runqueue worker)
  (throw tag-worker-run-task task))

(def function signal-worker-free (worker)
  "Signal the current worker that its now free"
  (clear-runqueue worker)
  (throw tag-worker-is-free nil))


(def function set-initial-worker-returned (initial-task)
  "Set the flag in the initial worker that the initial task had
returned"
  (let ((result (aref initial-task first-task-result))
        (worker (task-initial-worker initial-task)))
    (log-info "last-child of initial-task returned result = ~s" result)
    (setf (worker-initial-job-result worker) result)
    (setf (worker-initial-job-done worker) t)))


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
  (with-task-lock (parent)
    (assert (or (task-initial-worker parent)
                (member child (task-children parent))))
    (setf (task-children parent)
          (delete child (task-children parent)))
    (if (null (task-children parent))
        (if (task-initial-worker parent)
            (set-initial-worker-returned parent)
            (if (eq (task-state parent) :waiting)
                (signal-resume-task worker parent))))
    (signal-worker-free worker)))

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
            (log-debug "~d stolen from ~d task ~s" (worker-worker-num worker)
                       (worker-worker-num victim) (task-desc task))
            ;; steal was ok, promote the task by creating lock
            ;; at this point the pop-frame-check-failed path will be entered by the child
            ;; upon return, and it will block at worker lock
            (unless (task-lock task)
              (assert (null (task-children task)))
              (setf (task-lock task) (make-lock)))
            (incf (worker-runqueue-head victim))
            ;; cpu executing our child will get blocked in
            ;; pop-frame-check-falied, therefore its safe to
            ;; manipulate parent without lockingit
            (let ((child (aref (worker-tasks victim)
                               (worker-runqueue-head victim))))
              (add-task-child task child))
            task)))))

(def function add-task-child (task child)
  (declare (type task task child))
  (with-task-lock (task)
    (log-debug "~d parent ~s child ~s" 
               (worker-worker-num *worker*)
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

(def function run-stolen-task (worker task)
  "Runs a slow clone of the cilk procedure on this worker."
  ;; child pointer no longer needed, because we are now
  ;; on a different CPU and anything spawned on 
  (with-task-lock (task)
    ;; should already be marke as running on this CPU by the
    ;; steal-task protocol
    (assert (eq (task-state task)
                (worker-running-state worker))))
  ;; put into steal list
  (with-worker-lock (worker)
    ;; put into runqueue
    (setf (aref (worker-tasks worker)
                (incf (worker-runqueue-tail worker)))
          task))
  ;; finally run it
  (log-debug "~d running stolen task ~s" (worker-worker-num worker) (task-desc task))
  (task-returned worker task 
                 (funcall (the function (task-slow-clone task)) worker task)))

(def function pop-frame-check-failed (worker task)
  "Handles the case where task was stolen"
  (declare (type worker worker)
           (type task task))
  (log-debug "~d task ~s was stolen by someone" 
             (worker-worker-num worker) (task-desc task))
  (with-worker-lock (worker)
    ;; anything below us as stolen
    (fill (worker-tasks worker) nil :end (worker-runqueue-tail worker))
    ;; we are unwinding up to the previous do-worker loop,
    ;; so set the tail pointers to initial position
    (setf (worker-runqueue-tail worker) *initial-runqueue-tail*)
    ;; parent was stolen, so do the child-returned while parent
    ;; is on diff CPU protocol
    (child-returned worker task 
                    (aref (worker-tasks worker)
                          (worker-runqueue-head worker)))))

(def (function) pop-frame-check-fast (worker task)
  "Called after every spawn had returned"
  (declare (type worker worker)
           (type task task))
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
         (free-task worker))
        (t
         (pop-frame-check-failed worker task))))

(def (function) pop-frame-check-slow (worker task)
  "Called after every spawn had returned"
  (declare (type worker worker)
           (type task task))
  (cond ((eq (worker-ready-state worker)
             (my-compare-and-swap (task-state task)
                                  (worker-ready-state worker)
                                  (worker-running-state worker)))
         ;; successfully swapped the state from ready_CPU to
         ;; running_CPU therefore we are the only child, so safe to
         ;; just decrement child count and return
         ;; (log-debug "~d task ~s not stolen" (worker-worker-num worker) (task-desc task))
         ;; (assert (plusp (the fixnum (task-num-children task))))
         ;; (decf (the fixnum (task-num-children task)))

         ;; our parent is slow clone, which may may have other children
         ;; also finishing at the same time, therefore need to be atomic here
         (free-task worker))
        (t
         (pop-frame-check-failed worker task))))


(def function sync-check (worker task)
  ;; a running slow task should be the only task in this CPU runqueue
  (with-task-lock (task)
    (log-debug "~d task=~s length of child list = ~d" (worker-worker-num worker)
               (task-desc task)
               (length (task-children task)))
    (when (not (null (task-children task)))
      (setf (task-state task) :waiting)
      ;; reset the tail pointers before unwinding
      (with-worker-lock (worker)
        (setf (worker-runqueue-tail worker) *initial-runqueue-tail*))
      (throw tag-worker-is-free nil))))

