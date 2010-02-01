(in-package :cilk)

(in-root-suite) 

(defsuite* cilk-tests)

(defcilk fib (n)
  (declare (type fixnum n))
  (if (< n 2) n
      (let ((n (spawn (fib (- n 2))))
            (n2 (spawn (fib (- n 1)))))
        (declare (type fixnum n n2))
        (sync)
        (the fixnum (+ (the fixnum n) (the fixnum n2))))))

(deftest cilk-test-fib1 () 
  (kill-workers) 
  (start-worker)
  (start-worker) 
  (start-worker) 
  (sleep 0.1) 
  (is (= 9227465 (fib 35)))
  (kill-workers))


(defcilk fib1 (n)
  (declare (type fixnum n))
  (if (< n 2) n
      (let ((n (spawn (fib2 (- n 2))))
            (n2 (fib3 (- n 1))))
        (declare (type fixnum n n2))
        (sync)
        (the fixnum (+ (the fixnum n) (the fixnum n2))))))

(defcilk fib2 (n)
  (declare (type fixnum n))
  (if (< n 2) n
      (let ((n (fib3 (- n 2)))
            (n2 (spawn (fib1 (- n 1)))))
        (declare (type fixnum n n2))
        (sync)
        (the fixnum (+ (the fixnum n) (the fixnum n2))))))

(defun fib3 (n)
  (declare (type fixnum n))
  (if (< n 2) n
      (let ((n (fib1 (- n 2)))
            (n2 (fib2 (- n 1))))
        (declare (type fixnum n n2))
        (the fixnum (+ (the fixnum n) (the fixnum n2))))))

(deftest cilk-test-fib2 () 
  (kill-workers) 
  (start-worker)
  (start-worker) 
  (start-worker) 
  (sleep 0.1) 
  (is (= 9227465 (fib1 35)))
  (kill-workers))
