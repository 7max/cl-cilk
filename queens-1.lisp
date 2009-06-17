
;; board is respesented by a list
;; each element is queen position 
;; 
;; The 1st element of the list is row N and the last element
;; of the list is row 1


(deftype board () 'simple-vector)

(defun print-board (b)
  (declare (type board b))
  (let ((size (length b)))
    (iterate
      (for (the fixnum queen-column) in-vector b)
      (iterate (repeat queen-column)
         (write-char #\-))
      (write-char #\Q)
      (iterate (repeat (- size queen-column 1))
         (write-char #\-))
      (terpri))))

(defun ok (board pos)
  "See if any queens on the board would threaten a new queen
added as a new row "
  (declare (type board board)
           (type fixnum pos))
  (let ((newrow (length board)))
    (declare (type fixnum newrow))
    (flet ((threatens (row col)
             (declare (type fixnum row col))
             (when (or (= newrow row)
                       (= pos col)
                       (= (abs (- row newrow))
                          (abs (- col pos))))
               (return-from ok nil))))
      (iterate (for (the fixnum row) from (the fixnum 0))
               (for (the fixnum col) in-vector board)
               (threatens row col))
      t)))


(defun n-queens (limit)
  (labels ((n-queens-1 (board limit)
             (declare (type board board)
                      (type fixnum limit))
             (if (= (length board) limit)
                 (return-from n-queens board))
             (dotimes (i limit)
               (when (ok board i)
                 (let ((copy (make-array (1+ (length board)))))
                   (declare (dynamic-extent copy))
                   (replace copy board)
                   (setf (aref copy (length board))
                         i)
                   (n-queens-1 copy limit))))))
    (n-queens-1 (make-array 0) limit)))



(rewrite fib-inlet (n)
  (if (< n 2) n 
      (let ((accum 0))
        (spawn-let (res (spawn (fib-inlet (- n 2))))
                   (incf accum res))
        (spawn-let (res (spawn (fib-inlet (- n 2))))
                   (incf accum res))
        (sync)
        accum)))

(rewrite fib-inlet (n)
  (if (< n 2) n 
      (let ((accum 0))
        (flet ((add (res)
                 (incf accum res)))
          (spawn (fib-inlet (- n 2)))
          (spawn (fib-inlet (- n 2))))
        (sync)
        accum)))

(rewrite n-queens-1 (board limit)
  (declare (type board board)
           (type fixnum limit))
  (if (= (length board) limit)
      (return-from n-queens-1 board))
  (dotimes (i limit)
    (when (ok board i)
      (let ((copy (make-array (1+ (length board)))))
        (replace copy board)
        (setf (aref copy (length board))
              i)
        (after-spawn-let (result (n-queens-1 copy limit))
         ;; spawn had finished
         (when result
           (abort-siblings)
           (return result)))))))
