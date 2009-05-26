(cl:in-package :cilk)

(def macro multi-catch (tag-list &body forms)
  "Macro allowing catch of multiple tags at once and
finding out which tag was thrown. 

Returns (values RESULT TAG) where

RESULT is either the result of evaluationg FORMS or the value
thrown by the throw form.

TAG is NIl if evaluation of the FORMS completed normally
or the tag thrown and cought.

Example: 

(multiple-value-bind (result tag)
    (multi-catch (:a :b)
      ...FORMS...)
  (case tag 
    (:a ...)
    (:b ...)
    (t ...)))
"
  (let ((block-name (gensym)))
    `(block ,block-name
       ,(multi-catch-1 block-name tag-list forms))))

(def function multi-catch-1 (block-name tag-list forms)
  "Helper for multi-catch macro"
  (if (null tag-list) `(progn ,@forms)
      (let ((tmp (gensym)))
        `(let ((,tmp (catch ,(first tag-list)
                       (return-from ,block-name 
                         (values ,(multi-catch-1
                                   block-name (rest tag-list) forms))))))
           (return-from ,block-name (values ,tmp ,(first tag-list)))))))


(def macro catch-case (form &rest cases)
  (let ((tags (remove t (mapcar #'first cases)))
        (tag (gensym)))
    `(multiple-value-bind (it ,tag)
         (multi-catch ,tags ,form)
       (cond 
         ,@(loop for case in cases
              collect `(,(if (eq (first case) t) t  
                             `(eql ,tag ,(first case)))
                         ,@(rest case)))))))
