(cl:in-package :cilk)

;; Concepts
;;   
;;   Transform any lisp form into a 
;;
;;    (lambda (pc) 
;;      (let ((vars))
;;       (tagbody 
;;         ... forms ...
;;        )))
;;
;;   All the inner forms are walked and flattened into the single tagbody,
;;   for any inner (let) forms the variables are transferred into the very
;;   outer let, the (setq) is done for the variable initialization and
;;   code inside of (let) is flattened into our tagbody. Variable names
;;   are mygensymed and references inside are translated, so that the
;;   lexical scoping rules are preserved by all the variables having
;;   unique names.
;;
;;   Same flattenning process is done for any inner tagbody, progn, block
;;   etc forms.
;;
;;   The idea is that we can interrupt the flow of execution at any point
;;   and then jump right back into it, with all the state exactly the same
;;
;;   Any function calls are flattened by first evaluating all the arguments
;;   and assignning them to variables, and then calling the function.
;;
;;   (if) forms are flattenned by evaluating the condition and assigning
;;   it to the variable, and then using (go) to either execute or not
;;   execute THEN and ELSE branches.
;;
;;   (and) (or) forms are handled in the same manner
;;
;;   (block) with (return-from) is handled by havinga  label
;;
;;   Any form that (special-p) returns true for that is not handled 
;;   results in error
;;
;;   Optimizations:
;;     The form is not flattened if it does not contain any jump targets
;;     The variables inside the form are still rewritten if they refer
;;     to lexical variables bound in the parent scope
;; 

(def (class nc) cilk-special ()
  ((number :type fixnum)
   (after-label :type symbol)))

(def (class nc) spawn (cilk-special free-application-form)
  ())

(def (class nc) sync (cilk-special free-application-form)
  ())

(defvar *cilk-task-names* nil)
(defvar outer-let nil)
(defvar splice-into-fast nil)
(defvar splice-into-slow nil)
(defvar outer-tagbody-fast nil)
(defvar outer-tagbody-slow nil)
(defvar var-translations)
(defvar tag-translations)
(defvar receiver nil)
(defvar blocks nil)
(defvar unroll-list nil)
(defvar special-list nil)
(defvar worker-sym nil)
(defvar task-sym nil)
(defvar pc-sym nil)
(defvar vars nil)

(def generic unroll (form))
(def generic rewrite-references (form))
(def generic register-for-unroll (form))

(defvar *mygensym* 0)
(defun mygensym (&optional (prefix "G"))
  (intern (format nil "~A~D" prefix (incf *mygensym*)) :cilk))

(def macro def-unroller (class (&rest slots)  &body body)
  `(def method unroll ((form ,class))
     (with-slots ,slots form
       ,@body)))

(def macro def-rewriter (class (&rest slots)  &body body)
  `(def method rewrite-references ((form ,class))
     (with-slots ,slots form
       ,@body)))

(defgeneric walk-subforms (form func))

(def generic visit-form (form function)
  (:documentation "Apply function to the form and then
  recursively visit any children subforms"))

(def function visit-list (list func &key (key #'identity))
  (declare (type function key))
  (dolist (elem list)
    (visit-form (funcall key elem) func)))

(def method visit-form ((form form) func)
  (funcall func form)
  (flet ((maybe-visit-slot (slot)
           (when (and (slot-exists-p form slot)
                      (slot-boundp form slot))
             (visit-form (slot-value form slot) func)))
         (maybe-visit-slot-as-list (slot &optional (key #'identity))
           (when (and (slot-exists-p form slot)
                      (slot-boundp form slot))
             (visit-list (slot-value form slot) func :key key))))
    (maybe-visit-slot 'consequent)
    (maybe-visit-slot 'then)
    (maybe-visit-slot 'else)
    (maybe-visit-slot 'value)
    (maybe-visit-slot-as-list 'binds #'cdr)
    (maybe-visit-slot-as-list 'body)
    (maybe-visit-slot-as-list 'arguments))
  (values))

(def method visit-form ((form constant-form) func)
  (funcall func form))

(defmethod register-for-unroll ((form form))
  (values))

(def function mark-for-unrolling (form)
  (setf (gethash form unroll-list) t)
  (awhen (slot-value form 'parent)
    (mark-for-unrolling it))
  (values))

(def method register-for-unroll ((form free-application-form))
  "Decides if the form is a special form, after which a non-local
  entry into continuating is possible"
  (labels 
      ((ensure-just-one-spawn (form)
         (let ((cnt 0))
           (visit-form form 
                       (lambda (form)
                         (when (and (typep form 'free-application-form)
                                    (eq 'spawn (slot-value form 'operator)))
                           (incf cnt))))
           (when (> cnt 1)
             (error "More then one spawn call inside of ~s" form)))))
    (with-slots (operator arguments) form
      (when (eq operator 'spawn)
        (let* ((parent (slot-value form 'parent))
               (num (length special-list))
               (spawn (change-class form 'spawn 
                                    :number num
                                    :after-label (intern 
                                                  (format nil "AFTER-SPAWN-~d" num)
                                                  :keyword))))
          (push spawn special-list)
          (typecase parent
            (setq-form
             (ensure-just-one-spawn parent)
             ;; mark parent of the setq for unrolling, no need to
             ;; unroll setq itself, since it would simply introduces a
             ;; noop variable ie (setq orig-dest (spawn ...)) => (setq
             ;; tmp (spawn ...))  followed by (setq orig-dest tmp)
             (mark-for-unrolling (slot-value parent 'parent)))
            ((or let-form let*-form block-form progn-form tagbody-form)
             ;; stand-alone (spawn) in a progn-like form
             (ensure-just-one-spawn form)
             ;; mark the parent itself for the unrolling
             (mark-for-unrolling parent))
            (t (error "Spawn inside unsupported parent ~s" parent)))))
      (when (eq operator 'sync)
        (let* ((parent (slot-value form 'parent))
               (num (length special-list))
               (sync (change-class form 'sync 
                                   :number num
                                   :after-label (intern 
                                                 (format nil "AFTER-SYNC-~d" num)
                                                 :keyword))))
          (push sync special-list)
          (typecase parent
            ((or let-form let*-form block-form progn-form tagbody-form)
             ;; mark the parent itself for the unrolling
             (mark-for-unrolling parent))
            (t (error "sync inside unsupported parent ~s" parent))))))))

(def function requires-unrolling (form)
  "Checks if form was registered as special"
  (gethash form unroll-list))

(def generic maybe-unroll (form))

(def method maybe-unroll ((form form))
  "If form requires unrolling then unroll it, otherwise insert it
as is rewriting variable and tag references.

  The splicing of the form is done into both fast-tagbody and slow-tagbody
controlled by the variable splice-into-fast and splice-into-slow
"
  (cond ((requires-unrolling form)
         (let ((splice-into-fast t)
               (splice-into-slow t))
           (log-debug "unrolling in both ~s" form)
           (unroll form)))
        (t
         (let ((splice-into-fast splice-into-fast)
               (splice-into-slow splice-into-slow))
           (log-debug "not unrolling in both ~s" form)
           (rewrite-references form)
           (splice-form form)))))

(def method rewrite-references ((form form))
  "Rewrite the references in compound forms recursively "
  (labels  ((maybe-visit-slot (slot)
              (when (and (slot-exists-p form slot)
                         (slot-boundp form slot))
                (rewrite-references (slot-value form slot))))
            (maybe-visit-slot-as-list (slot)
              (when (and (slot-exists-p form slot)
                         (slot-boundp form slot))
                (dolist (elem (slot-value form slot))
                  (rewrite-references elem)))))
    (maybe-visit-slot 'consequent)
    (maybe-visit-slot 'then)
    (maybe-visit-slot 'else)
    (maybe-visit-slot 'value)
    (maybe-visit-slot-as-list 'body)))

(def method rewrite-references ((form constant-form)))

(def-rewriter free-application-form (arguments)
  "Rewrite references in (OP ... ARGS)"
  (mapc #'rewrite-references arguments))

(def-rewriter setq-form (var value)
  "Rewrite substituted variable value, in case LET form for that
variable was unrolled"
  (awhen (assoc var var-translations)
    (setf var (rest it)))
  (rewrite-references value))

(def-rewriter go-tag-form (name)
  "Rewrite substituted GO tags in case inner TAGBODY was
unrolled"
  (awhen (assoc name tag-translations)
    (setf name (rest it))))

(def-rewriter go-form (name)
  "Rewrite substituted GO tags in case inner TAGBODY was
unrolled"
  (awhen (assoc name tag-translations)
    (setf name (rest it))))

;; (def-unroller go-tag-form (name)
;;   (let ((receiver nil))
;;     (splice-form form)))

;; (def-unroller go-form (name)
;;   (let ((receiver nil))
;;     (splice-form form)))

(def-rewriter tagbody-form ()
  (let ((tag-translations nil))
    (call-next-method)))

(def-rewriter let-form (binds declares body)
  (let ((var-translations var-translations)
        (temp-tranlations))
      (dolist (i binds)
        (let* ((name (first i))
               (value (rest i)))
          (push (cons name name) temp-tranlations)
          (rewrite-references value)))
      (setq var-translations (append temp-tranlations var-translations))
      (mapc #'rewrite-references body)))

(def-rewriter let*-form (binds declares body)
  (let ((var-translations var-translations))
      (dolist (i binds)
        (let* ((name (first i))
               (value (rest i)))
          (push (cons name name) var-translations)
          (rewrite-references value)))
      (mapc #'rewrite-references body)))

(def-rewriter local-variable-reference (name)
  (awhen (assoc name var-translations)
    (setf name (rest it))))

(def-rewriter flet-form (binds)
  (dolist (elem binds)
    (rewrite-references (cdr elem)))
  (call-next-method))

(def-rewriter labels-form (binds)
  (dolist (elem binds)
    (rewrite-references (cdr elem)))
  (call-next-method))

(def-rewriter lambda-function-form (arguments)
  (let ((var-translations var-translations)
        (temp-tranlations))
    (dolist (i arguments)
      (assert (typep i 'function-argument-form))
      (with-slots (name) i
        (push (cons name name) temp-tranlations)
        (rewrite-references i)))
    (setq var-translations (append temp-tranlations var-translations))
    (call-next-method)))

(def-rewriter optional-function-argument-form (default-value)
  (rewrite-references default-value))

(def-rewriter keyword-function-argument-form (default-value)
  (rewrite-references default-value))

(def function %defcilk (name args body)
  ;; find first non-string
  (let (declares)
    (awhen (find-if-not #'stringp body)
      (when (and (listp it)
                 (eq (first it) 'declare))
        (setf body (remove it body))
        (setf declares it))
      (rewrite-cilk-form name  
                         `(lambda ,args 
                            ,declares
                            (block ,name ,@body))))))

(def function generate-dispatch ()
  `(case ,pc-sym
     ,@(loop for form in special-list
          as label = (slot-value form 'after-label)
          collect `(,label (go ,label)))))


(def (class nc) closed-over-var ()
  ((name :type symbol)
   (value :type form)
   (index :type fixnum)
   (declared-type :type (or symbol list)))
  (:documentation "Contains information about a closed over variable"))

(def function extract-declare (name declares)
  "Extract a declared type for a variable name from an list of
declare forms"
  (awhen (find-if 
          (lambda (form)
            (and (typep form 'arnesi::type-declaration-form)
                 (equal name (slot-value form 'name))))
          declares)
    (slot-value it 'type-form)))

(def function closed-over-vars-from-let (form)
  (with-slots (binds declares) form 
    (mapcar (lambda (var)
              (destructuring-bind (name . value) var
                (new 'closed-over-var 
                     :name name 
                     :value value
                     :declared-type 
                     (extract-declare name declares))))
            binds)))

(def function closed-over-vars-from-lambda (form)
  (with-slots (arguments declares) form
    (mapcar (lambda (arg)
              (let ((name (name arg)))
                (new 'closed-over-var
                     :name name
                     :declared-type 
                     (extract-declare name declares)))) arguments)))

(defun rewrite-cilk-form (name lambda-form)
  ;; setup the variables 
  ;; TODO global to simplify debugging, bind most of these
  (setq outer-tagbody-fast (new 'tagbody-form 
                                :body nil))
  (setq outer-tagbody-slow (new 'tagbody-form 
                                :body nil))
  (setq outer-let (new 'let-form 
                       :binds nil
                       :declares nil
                       :body nil))
  
  (setq var-translations nil)
  (setq tag-translations nil)
  (setq special-list nil)
  (setq vars nil)
  ;; parse teh form
  (assert (eq (first lambda-form) 'lambda))
  (let* ((receiver (new-var "OUTER-TAGBODY-RECEIVER"))
         (unroll-list (make-hash-table))
         (lform (walk-form lambda-form))
         (form (first (slot-value lform 'body)))
         (args (second lambda-form)))
    ;; collect the spawn/sync points and mark all forms containing
    ;; them for unrolling
    (visit-form form #L (progn
                          (register-for-unroll !1)))
    ;; below marks any inner blocks that possibly execute non-local exit
    ;; from one of the unrolled blocks
    ;;
    ;; This is needed because unrolled blocks don't exist anymore as separate
    ;; blocks and are instead merged into the outer tagbody, so return-from
    ;; from such block needs to be converted into a GO form, nessesitating
    ;; that everything down gets merged into outer tagbody also
    ;;
    ;; TODO actually GO can do non-local transfer upward also, so 
    ;; there is no need to unroll the inner block containing RETURN-FROM
    ;; instead RETURN-FROM should be rewritten as a GO but the block
    ;; can be left in place
    (visit-form form #L (when (and 
                               (typep !1 'return-from-form)
                               (requires-unrolling (slot-value !1 'target-block)))
                          (log-debug "marking ~s" !1)
                          (mark-for-unrolling !1)))
    (log-debug "table now = ~s" unroll-list)
    (let ((worker-sym (mygensym "WORKER"))
          (task-sym (mygensym "TASK"))
          (pc-sym)
          (name-fast (make-fast-name name))
          (name-slow (make-slow-name name)))
      ;; add PC 
      (setf pc-sym (new-var "PC" 'symbol))
      ;; add variables representing the parameter list
      ;; to the closed over vars list
      (with-slots (arguments declares) lform
        (dolist (arg arguments)
          (let ((name (name arg)))
            (add-task-new-var 
             (new 'closed-over-var
                  :name name
                  :declared-type 
                  (extract-declare name declares))))))
      ;; process the form, collecting the closed over vars
      (maybe-unroll form)
      ;; splice the the outer LET containing closure variables, and
      ;; generate the full body of the cilk procedure
      (let* ((outer-block (mygensym "outer-block"))
             (parent (mygensym "parent"))
             (parent-spawn-num (mygensym "parent-spawn-num"))
             (task-size (+ first-task-result (length vars)))
             (var-macrolets (mapcar (lambda (var)
                                      (aif (declared-type-of var)
                                           `(,(name-of var)
                                              (the ,it (aref ,task-sym ,(index-of var))))
                                           `(,(name-of var) (aref ,task-sym ,(index-of var)))))
                                    vars))
             (fast-block (rest (unwalk-form outer-tagbody-fast))) ;strip leading TAGBODY
             (slow-block (rest (unwalk-form outer-tagbody-slow))) ;ditto
             )
        (setf fast-block
              `(block ,outer-block 
                 (tagbody
                    ,@fast-block
                    (return-from ,outer-block ,receiver))))
        (setf slow-block
              `(block ,outer-block
                 (tagbody
                    ,(generate-dispatch)
                    ,@slow-block
                    (return-from ,outer-block ,receiver))))
        `(progn 
           ;; fast clone
           (defun ,name-fast (,worker-sym ,parent ,parent-spawn-num ,@args)
             (declare (type worker ,worker-sym)
                      (type simple-vector ,parent)
                      (type fixnum ,parent-spawn-num)
                      ,@(iterate (for arg in (slot-value lform 'arguments))
                                 (for name = (name arg))
                                 (for var = (find name vars :key #'name-of))
                                 (aif (declared-type-of var)
                                      (collect `(type ,it ,name)))))
             (let ((,task-sym (alloc-task ,worker-sym ,task-size ,parent ,parent-spawn-num (function ,name-slow))))
               (declare (type simple-vector ,task-sym))
               ,(when *cilk-task-names*
                      `(setf (task-name ,task-sym) 
                             (list ',name
                                   ,@args)))
               ;; #+cilk-status
               (setf (task-state ,parent) (worker-ready-state ,worker-sym))
               (incf (worker-runqueue-tail ,worker-sym))
               ;; store the arguments in the task structure
               ,@(iterate (for arg in (slot-value lform 'arguments))
                          (for name = (name arg))
                          (for var = (find name vars :key #'name-of))
                          (for idx = (index-of var))
                          (aif (declared-type-of var)
                               (collect `(setf (the ,it (aref ,task-sym ,idx)) ,name))
                               (collect `(setf (aref ,task-sym ,idx) ,name))))
               (prog1
                   (symbol-macrolet ,var-macrolets 
                     ,fast-block)
                 (setf (task-state ,task-sym) :done-fast))))
           ;; slow clone
           (defun ,name-slow (,worker-sym ,task-sym)
             (declare (type worker ,worker-sym)
                      (type simple-vector ,task-sym))
             (prog1 (symbol-macrolet ,var-macrolets 
                      ,slow-block)
               (setf (task-state ,task-sym) :done-slow)))
           (defun ,name (,@args)
             ;; normal function creates a special parent task
             ;; then runs it on the current worker and returns result.
             (let ((worker *worker*))
               (cond 
                 (worker 
                  (flet ((doit (worker parent)
                           (,name-fast worker parent 
                                       first-task-result ,@args)))
                    (declare (dynamic-extent #'doit))
                    (do-recursive-worker
                        worker
                      #'doit)))
                 (t (flet ((doit (worker parent)
                             (,name-fast worker parent 
                                         first-task-result ,@args)))
                      (declare (dynamic-extent #'doit))
                      (start-worker #'doit)))))))))))


(def function new (class &rest initargs)
  (apply #'make-instance class initargs))

(defun class-name-of (obj)
  (class-name (class-of obj)))

(defgeneric splice-form (form))

(def function nil-p (form)
  "Check if form is a NIL"
  (and (typep form 'free-variable-reference)
       (eq (slot-value form 'name) nil)))

(defmethod splice-form ((form form))
  :documentation "Appends form to the end of the body of the
  outer tagbody form. If receiver is bound, then our caller is
  waiting for our result, and setq form is generated to assign
  result of this form to the receiver "
  (flet ((frob (outer-tagbody)
           (with-slots (body) outer-tagbody
             (if receiver
                 (setf body (append body (list (new 
                                                'setq-form 
                                                :var receiver 
                                                :value form))))
                 (unless (nil-p form)
                   (setf body (append body (list form))))))))
    (when splice-into-fast (frob outer-tagbody-fast))
    (when splice-into-slow (frob outer-tagbody-slow))))

(defmethod splice-form ((form-list list))
  (flet ((frob (outer-tagbody)
           (with-slots (body) outer-tagbody
             (if receiver
                 (setf body (append body (butlast form-list)
                                    (list (new 'setq-form 
                                               :var receiver 
                                               :value (first (last form-list))))))
                 (setf body (append body form-list))))))
    (when splice-into-fast (frob outer-tagbody-fast))
    (when splice-into-slow (frob outer-tagbody-slow))))


(defun make-fast-name (name)
  "Give a symbol for function implemenitng spawn fast clone"
  (intern (format nil "_~a-FAST-CLONE" name)))

(defun make-slow-name (name)
  "Give a symbol for function implemenitng spawn slow clone"
  (intern (format nil "_~a-SLOW-CLONE" name)))

(def function splice-spawn-call (outer-tagbody result-var spawn-form is-fast-clone)
  "Splice the (setq target (spawn FOO args)) call into the
tagbody replacing the spawn with a (progn (call fast
clone) (pop-frame-check))"
  (declare (ignore is-fast-clone))
  (with-slots (body) outer-tagbody
    (with-slots (after-label) spawn-form
      (let* ((result-idx (if result-var
                           (aif (find result-var vars :key #'name-of)
                                (index-of it)
                                (error "Variable ~s not found in closed over vars" result-var))
                           first-task-result))
             ;; extract the call in (spawn (foo args))
             (spawn-target (first (slot-value spawn-form 'arguments)))
             ;; foo
             (spawn-name (slot-value spawn-target 'operator))
             ;; args
             (spawn-args (slot-value spawn-target 'arguments))
             (spawn-call 
              (new 'free-application-form 
                   :operator (make-fast-name spawn-name)
                   :arguments 
                   `(,(new 'local-variable-reference :name worker-sym)
                      ,(new 'local-variable-reference :name task-sym)
                      ,(new 'constant-form :value result-idx)
                      ,@spawn-args))))
        (setf body 
              (append body
                      (list 
                       (new 'setq-form :var pc-sym :value 
                            (new 'constant-form :value after-label))
                       (if result-var 
                           (new 'setq-form :var result-var
                                :value spawn-call)
                           spawn-call)
                       (new 'free-application-form 
                            :operator 'pop-frame-check
                            :arguments 
                            `(,(new 'local-variable-reference :name worker-sym)
                               ,(new 'local-variable-reference :name task-sym)))
                       (new 'go-tag-form :name after-label))))))))

(defmethod splice-form ((form spawn))
  :documentation "Appends form to the end of the body of the
  outer tagbody form. If receiver is bound, then our caller is
  waiting for our result, and setq form is generated to assign
  result of this form to the receiver "
  (flet ((frob (outer-tagbody is-fast-clone)
           (with-slots (body) outer-tagbody
             (cond ((and receiver 
                          (typep (slot-value form 'parent)
                                 '(or let-form let*-form)))
                    (splice-spawn-call outer-tagbody receiver form is-fast-clone))
                   (t
                    (log-warn "Ignoring receiver ~s for spawn not in let form" receiver)
                    (splice-spawn-call outer-tagbody nil form is-fast-clone))))))
    (when splice-into-fast (frob outer-tagbody-fast t))
    (when splice-into-slow (frob outer-tagbody-slow nil))))

(defmethod splice-form ((form setq-form))
  :documentation "Appends form to the end of the body of the
  outer tagbody form. If receiver is bound, then our caller is
  waiting for our result, and setq form is generated to assign
  result of this form to the receiver "
  (if (not (typep (slot-value form 'value) 'spawn))
      (call-next-method)
      (flet ((frob (outer-tagbody is-fast-clone)
               (with-slots (body) outer-tagbody
                 (when receiver
                     (log-warn "Ignoring ceceiver ~s set for setq spawn form" receiver))
                 (splice-spawn-call outer-tagbody 
                                    (slot-value form 'var)
                                    (slot-value form 'value)
                                    is-fast-clone))))
        (when splice-into-fast (frob outer-tagbody-fast t))
        (when splice-into-slow (frob outer-tagbody-slow nil)))))


(def function splice-sync-call (outer-tagbody form is-fast-clone)
  (with-slots (body) outer-tagbody 
    (with-slots (after-label number) form
      (setq body 
            (append body
                    (if is-fast-clone
                        (list 
                         (new 'go-tag-form :name after-label))
                        (list 
                         (new 'setq-form :var pc-sym :value 
                              (new 'constant-form :value after-label))
                         (new 'free-application-form :operator 'sync-check
                              :arguments 
                              `(,(new 'local-variable-reference :name worker-sym)
                                 ,(new 'local-variable-reference :name task-sym)))
                         (new 'go-tag-form :name after-label))))))))

(defmethod splice-form ((form sync))
  :documentation "Appends form to the end of the body of the
  outer tagbody form. If receiver is bound, then our caller is
  waiting for our result, and setq form is generated to assign
  result of this form to the receiver "
  (flet ((frob (outer-tagbody is-fast-clone)
           (with-slots (body) outer-tagbody
             (when receiver
               (log-warn "Ignoring receiver ~s for sync form" receiver))
             (splice-sync-call outer-tagbody form is-fast-clone))))
    (when splice-into-fast (frob outer-tagbody-fast t))
    (when splice-into-slow (frob outer-tagbody-slow nil))))

(defun add-task-new-var (var)
  "Add a variable to vars global list, giving it an index"
  (declare (type closed-over-var var))
  (setf (index-of var) (+ first-task-result (length vars)))
  (push var vars))

(defun new-var (&optional arg type)
  (let ((name (mygensym arg)))
    ;; TODO put type into declares
    (when type
      ())
    (with-slots (binds) outer-let
      (push 
       (cons name 
             (new 'free-variable-reference :name nil))
       binds)
      (add-task-new-var 
       (new 'closed-over-var 
            :name name 
            ;; :value nil
            :declared-type 
            type)))
    name))

(def-unroller form ()
  (splice-form form))

(def-unroller progn-form (body)
  (mapcar #'maybe-unroll body))

(def-unroller block-form (name body)
  (let ((after-block (mygensym "AFTER-BLOCK"))
        (blocks blocks)
        (res-var (new-var "BLOCK-RESULT")))
    (push (list form res-var after-block) blocks)
    (let ((receiver nil))
      (mapc #'maybe-unroll (butlast body)))
    (let ((receiver res-var))
      (mapc #'maybe-unroll (last body)))
    (let ((receiver nil))
      (splice-form (new 'go-tag-form :name after-block)))
    (when receiver 
      (splice-form (new 'local-variable-reference :name res-var)))))

(def-unroller return-from-form (target-block result)
  (assert target-block)
  (let ((b (assoc target-block blocks)))
    (assert b)
    (let ((receiver (second b)))
      (maybe-unroll result))
    (let ((receiver nil))
      (splice-form (new 'go-form :name (third b))))))

(def-unroller tagbody-form (body)
  (let ((tag-translations tag-translations))
    (dolist (tag (remove-if-not (lambda (form) (typep form 'go-tag-form)) body))
      (with-slots (name) tag
        (log-debug "Adding tag translation for ~s" name)
        (push (cons name (mygensym (symbol-name name)))
              tag-translations)))
    (mapcar #'maybe-unroll body)))

(def-unroller setq-form (var value)
  (let ((temp-var (new-var "SETQ")))
    (let ((receiver temp-var))
      (maybe-unroll value))
    (splice-form (new 'setq-form :var (or (cdr (assoc var var-translations)) var)
                      :value (new 'local-variable-reference
                                  :name temp-var)))))

(def-unroller free-application-form (operator arguments)
  (let ((temp-vars (mapcar (lambda (arg) (declare (ignore arg))
                                   (new-var "PARAM"))
                           arguments)))
    (loop 
       for arg in arguments
       for var in temp-vars
       do (let ((receiver var))
            (maybe-unroll arg)))
    (splice-form (new 'free-application-form :operator operator
                      :arguments (loop for var in temp-vars
                                      collect (new 'local-variable-reference
                                                   :name var))))))

(def-unroller local-variable-reference (name)
  (splice-form (aif (assoc name var-translations)
                    (new 'local-variable-reference :name (rest it))
                    form)))

(def-unroller let-form (binds declares body)
  (let ((var-translations var-translations)
        (temp-tranlations))
      (dolist (i binds)
        (let* ((name (first i))
               (value (rest i))
               ;; (type (find name declares :key (lambda (d)
               ;;                                  (slot-value d 'name))))
               (new-name (new-var (symbol-name name))))
          (let ((receiver new-name))
            (maybe-unroll value))
          (push (cons name new-name) temp-tranlations)))
      (setq var-translations (append temp-tranlations var-translations))
      (log-debug "Before unrolling let form var-transations ~s" var-translations)
      (mapcar #'maybe-unroll body)))

(def-unroller let*-form (binds declares body)
  (let ((var-translations var-translations))
      (dolist (i binds)
        (let* ((name (first i))
               (value (rest i))
               (new-name (new-var (symbol-name name))))
          (let ((receiver new-name))
            (maybe-unroll value))
          (push (cons name new-name) var-translations)))
      (mapcar #'maybe-unroll body)))

(def-unroller if-form (consequent then else)
  (let* ((lab1 (mygensym "IFELSE"))
         (lab2 (mygensym "IFEND"))
         (unroll-cond? (requires-unrolling consequent))
         (var1 (when unroll-cond? (new-var "IF-COND"))))
    (if unroll-cond? 
        (let ((receiver var1))
          (unroll consequent))
        (rewrite-references consequent))
    (let ((receiver nil))
      (splice-form (new 'if-form 
                        :consequent 
                        (new 'free-application-form
                             :operator 'not
                             :arguments (list (if unroll-cond? 
                                                  (new 'local-variable-reference :name  var1)
                                                  consequent)))
                        :then (new 'go-form :name lab1)
                        :else (new 'free-variable-reference :name nil))))
    (maybe-unroll then)
    (let ((receiver nil))
      (splice-form (new 'go-form :name lab2))
      (splice-form (new 'go-tag-form :name lab1)))
    (maybe-unroll else)
    (let ((receiver nil))
      (splice-form (new 'go-tag-form :name lab2)))))

(def (macro e) defcilk (name (&rest lambda-list) &body body)
  (%defcilk name lambda-list body))
