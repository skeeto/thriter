;;; thriter-tests.el --- tests for thriter -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'thriter)
(require 'generator)

(thriter-defun counter-generator (n)
  (dotimes (i n)
    (thriter-yield i)))

(thriter-defun list-generator (list)
  (dolist (e list)
    (thriter-yield e)))

(ert-deftest thriter ()
  (let ((iter (counter-generator 10)))
    (should (equal (number-sequence 0 9)
                   (cl-loop for i = (thriter-next iter)
                            while (car i)
                            collect (cdr i))))))

(ert-deftest thriter-do ()
  (let ((result ()))
    (thriter-do (v (counter-generator 9))
      (push v result))
    (should (equal (number-sequence 0 8)
                   (nreverse result)))))

(ert-deftest thriter-close ()
  (let* ((iter (list-generator (list :a :b :c)))
         (thread (thriter--thread (car iter))))
    (should (equal '(t . :a)
                   (thriter-next iter)))
    (thriter-close iter)
    (should-not (thread-alive-p thread))))

(ert-deftest thriter-finalize ()
  (let* ((iter (counter-generator 100))
         (thread (thriter--thread (car iter))))
    (dotimes (_ 10)
      (thriter-next iter))
    ;; Force finalizer to run
    (setf (cdr iter) nil)
    (garbage-collect)
    ;; Should be safe to join the thread now
    (thread-join thread)
    (should-not (thread-alive-p thread))))

;; Benchmark:

(iter-defun counter-generator-iter (n)
  (dotimes (i n)
    (iter-yield i)))

(defun thriter-benchmark ()
  (interactive)
  (princ
   (format "iter    %s\n"
           (benchmark-run 10
             (iter-do (i (counter-generator-iter 10000))
               (null i)))))
  (princ
   (format "thriter %s\n"
           (benchmark-run 10
             (thriter-do (i (counter-generator 10000))
               (null i))))))

;;; thriter-tests.el ends here
