;;; thriter.el --- thread-based generators -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Code:

(require 'cl-lib)

(cl-defstruct (thriter- (:constructor thriter--create-1)
                        (:predicate nil)
                        (:copier nil))
  (thread nil)
  (mutex nil :read-only t)
  (condvar-iterator nil :read-only t)
  (waiting-iterator t)
  (result-iterator nil)
  (condvar-caller nil :read-only t)
  (waiting-caller t)
  (result-caller nil)
  (box nil :read-only t))

(defvar thriter--state)

(defvar thriter--done (make-symbol "done")
  "Sentinel value returned by an iterator when it completes.")

(defun thriter--create ()
  "Create a fresh iterator state without a thread."
  (let ((mutex (make-mutex)))
    (thriter--create-1
     :mutex mutex
     :condvar-iterator (make-condition-variable mutex)
     :condvar-caller (make-condition-variable mutex)
     :box (cons nil nil))))

(defun thriter--make-finalizer (thread)
  "Create a finalizer to destroy the iterator running in THREAD."
  (make-finalizer (lambda () (thread-signal thread 'error nil))))

(defun thriter--make-thread (state func args)
  "Start an iterator thread for the given STATE, returning an iterator."
  (setf (thriter--thread state)
        (make-thread
         (lambda ()
           (let ((thriter--state state))
             ;; Wait for first "next" call
             (with-mutex (thriter--mutex state)
               (while (thriter--waiting-iterator state)
                 (condition-wait (thriter--condvar-iterator state)))
               (setf (thriter--waiting-iterator state) t))
             ;; Start iterator
             (apply func args)
             (thriter-yield thriter--done)))))
  (cons state
        (thriter--make-finalizer (thriter--thread state))))

(defun thriter-next (iter &optional yield-result)
  "Return the next value from the iterator.

The return value is a cons cell. The car indicates if the
iterator returned a value (t) or if it completed (nil). The cdr
is the actual return value."
  (let ((state (car iter)))
    (with-mutex (thriter--mutex state)
      (setf (thriter--result-iterator state) yield-result
            (thriter--waiting-iterator state) nil)
      (condition-notify (thriter--condvar-iterator state))
      (while (thriter--waiting-caller state)
        (condition-wait (thriter--condvar-caller state)))
      (setf (thriter--waiting-caller state) t)
      (let ((result (thriter--result-caller state))
            (box (thriter--box state)))
        (prog1 box
          (if (eq result thriter--done)
              (setf (car box) nil
                    (cdr box) nil)
            (setf (car box) t
                  (cdr box) result)))))))

(defun thriter-yield (value)
  "When inside an iterator, yield VALUE to the caller."
  (let ((state thriter--state))
    (with-mutex (thriter--mutex state)
      (setf (thriter--result-caller state) value
            (thriter--waiting-caller state) nil)
      (condition-notify (thriter--condvar-caller state))
      (while (thriter--waiting-iterator state)
        (condition-wait (thriter--condvar-iterator state)))
      (setf (thriter--waiting-iterator state) t)
      (thriter--result-iterator state))))

(defmacro thriter-lambda (arglist &rest body)
  "Produces an unnamed generator function.
Requires lexical scope."
  (declare (indent defun))
  (cl-assert lexical-binding)
  (let ((args-sym (make-symbol "args")))
    `(lambda (&rest ,args-sym)
       (thriter--make-thread
        (thriter--create)
        (lambda ,arglist ,@body)
        ,args-sym))))

(defmacro thriter-defun (name arglist &rest body)
  "Produces a named generator function.
Requires lexical scope."
  (declare (indent defun))
  `(defalias ',name (thriter-lambda ,arglist ,@body)))

(defun thriter-close (iter)
  "Immediately destroy iterator ITER, freeing allocated resources."
  (let ((thread (thriter--thread (car iter))))
    (thread-signal thread 'error nil)
    (thread-join thread)))

(defmacro thriter-do (var-and-iter &rest body)
  "Like `dolist', but for iterators."
  (declare (indent defun))
  (let ((result-sym (make-symbol "result"))
        (iter-sym (make-symbol "iter")))
    `(let* ((,iter-sym ,(cadr var-and-iter))
            (,result-sym nil)
            (,(car var-and-iter)))
       (while (car (setf ,result-sym (thriter-next ,iter-sym)))
         (setf ,(car var-and-iter) (cdr ,result-sym))
         ,@body))))

(defun thriter-yield-from (iter)
  (thriter-do (v iter)
    (thriter-yield v)))

(provide 'thriter)

;;; thriter.el ends here
