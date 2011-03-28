#+:xyzzy
(require 'cmu_loop)

(defpackage :brackets
  (:use #-xyzzy :cl #+xyzzy :lisp))


(in-package :brackets)

(export '(enable-brackets
	  disable-brackets))

(defvar *brackets-readtable* nil)
(defvar *backup-readtable* nil)


(defun bracket-reader (stream char)
  (declare (ignore char))
  (let ((^ (intern "^")) (_ (intern "_")))
    `(lambda (#0=#:_)
       (labels ((,^ (,_) ,(read-delimited-list #\] stream t)))
	 (,^ #0#)))))

(defun sharp-bracket-reader (stream char n)
  (declare (ignore char))
  (let ((args (loop for n from 1 to (or n 0)
		collect (intern (format nil "_~A" n))))
	(body (read-delimited-list #\] stream t)))
    `(lambda ,args ,body)))


(defun enable-brackets ()
  (unless #1=*brackets-readtable*
    (setq #1# (copy-readtable))
    (set-macro-character #\[ #'bracket-reader nil #1#)
    (set-dispatch-macro-character #\# #\[ #'sharp-bracket-reader #1#)
    (set-syntax-from-char #\] #\) #1# #1#)
    #+:xyzzy
    (ed:set-syntax-match ed::*lisp-mode-syntax-table* #\[ #\])
    (setq *backup-readtable* (copy-readtable)))
  (setq *readtable* #1#)
  t)

(defun disable-brackets ()
  (if *backup-readtable*
      (setq *readtable* *backup-readtable*)
    nil))

(provide 'brackets)
