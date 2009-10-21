;;; utilities from Arc

#+xyzzy
(require 'cmu_loop)

(in-package #+xyzzy "user"
	    #-xyzzy "common-lisp-user")

(defmacro falias (f g)
  `(setf (symbol-function ',f)
	 (symbol-function ',g)))

;;; functions
(defun idfn (x) x)

(defun testify (x)
  (if (functionp x) x
    #'(lambda (a) (equal x a))))

(defun mem (test seq)
  (member-if (testify test) seq))

(defun pos (test seq)
  (position-if (testify test) seq))

(defun rm (test seq)
  (remove-if (testify test) seq))

(defun keep (test seq) 
  (remove-if-not (testify test) seq))

(defun pair (xs &optional (f #'list))
  (cond ((null xs)
	 nil)
	((null (cdr xs))
	 (list (list (car xs))))
	(t (cons (funcall f (car xs) (cadr xs))
		 (pair (cddr xs) f)))))

(defun firstn (n xs)
  (if (null n)
      xs
    (if (and (> n 0) xs)
	(cons (car xs) (firstn (- n 1) (cdr xs)))
      nil)))

(defun tuples (xs &optional (n 2))
  (if (null xs)
      nil
    (cons (firstn n xs)
	  (tuples (nthcdr n xs) n))))

(defun alref (al key)
  (cadr (assoc key al :test #'equal)))

(defun range (start end)
  (loop for i from start to end
    collect i))


;; io
(defun pr (&rest objs)
  (format t "~{~A~}" objs)
  (car objs))

(defun prt (&rest objs)
  (mapc #'(lambda (o) (if o (princ o))) objs)
  (car objs))

(defun prn (&rest objs)
  (format t "~{~A~}~%" objs)
  (car objs))

(defun prs (objs)
  (format t "~{~A~^ ~}" objs)
  objs)

(defun prall (objs &optional (h "") (s ","))
  (let ((fmt (format nil "~A~~{~~A~~^~A~~}" h s)))
    (format t fmt objs)
    objs))

(defun readb (&optional (is *standard-input*))
  (read-byte is nil nil))

(defun readc (&optional (is *standard-input*))
  (read-char is nil nil))

(falias instring make-string-input-stream)
(falias outstring make-string-output-stream)
(falias inside get-output-stream-string)


;; macros
(defmacro zap (op place &rest args)
  `(setf ,place (apply ,op ,place ,args)))

(defmacro w/uniq (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym))) syms)
     ,@body))

(defmacro with (params &body body)
  (let ((pairs (pair params)))
    `(let ,pairs
       ,@body)))

(defmacro withs (params &body body)
  (let ((pairs (pair params)))
    `(let* ,pairs
       ,@body)))

(defmacro accum (accfn &body body)
  (w/uniq (gacc item)
    `(let (,gacc)
       (labels ((,accfn (,item)
		  (push ,item ,gacc)))
	 ,@body)
       (nreverse ,gacc))))

(defmacro drain (expr &optional eof)
  (w/uniq (gv gtest gacc)
    `(let ((,gtest (testify ,eof)) ,gacc)
       (do ((,gv ,expr ,expr))
	   ((funcall ,gtest ,gv) (nreverse ,gacc))
	 (push ,gv ,gacc)))))

(defmacro n-of (counts &rest exprs)
  (w/uniq (g)
    `(loop for ,g from 1 to ,counts
       ,@(mapcan #'(lambda (expr) `(collect ,expr))
		 exprs))))

#+nil
(defmacro in (x &rest choices)
  (w/uniq (g)
    `(with (,g ,x)
       (or ,@(mapcar #'(lambda (c) `(equal ,g ,c)) choices)))))

(defmacro rfn (name params &body body)
  `(labels ((,name ,params ,@body))
     #',name))

(defmacro afn (params &body body)
  `(labels ((self ,params ,@body))
     #'self))

#|
(defmacro aloop (start test update &body body)
  (w/uniq (gfn gparm)
    `(with ,start
       (funcall (rfn ,gfn (,gparm)
		  (when ,gparm
		    ,@body ,update (,gfn ,test)))
		,test))))

(defmacro for (v init max &body body)
  (w/uniq (gi gm)
    `(with (,v nil ,gi ,init ,gm (+ ,max 1))
       (aloop (setq ,v ,gi) (< ,v ,gm) (incf ,v)
	 ,@body))))
|#
(defmacro aloop (start test update &body body)
  `(progn ,start
     (loop while ,test
       do (progn ,@body ,update))))

(defmacro for (v init max &body body)
  (w/uniq (gv)
    `(do ((,v ,init (1+ ,v)))
	 ((< ,max ,v))
       ,@body)))

#|
(mac down (v init min . body)
  (w/uniq (gi gm)
    `(with (,v nil ,gi ,init ,gm (- ,min 1))
       (aloop (setq ,v ,gi) (> ,v ,gm) (decf ,v)
         ,@body))))
|#

(defmacro repeat (n &body body)
  `(for ,(gensym) 1 ,n ,@body))

(defmacro whilet (var test &body body)
  `(do ((,var ,test ,test))
       ((not ,var))
     ,@body))



;;; hash
(defmacro dohash ((var hash &optional result) &body body)
  `(progn
     (maphash #'(lambda ,var ,@body) ,hash)
     ,result))

(defun keys (h)
  (accum a (dohash ((k v) h) (a k))))
 
(defun vals (h)
  (accum a (dohash ((k v) h) (a v))))


;;; io-macro
(defmacro w/infile ((var file . rest) &body body)
  `(with-open-file (,var ,file :direction :input :if-does-not-exists :error)
     ,@(if rest `((w/infile ,rest ,@body))
	 body)))

(defmacro w/outfile ((var file . rest) &body body)
  `(with-open-file (,var ,file :direction :output :if-exists :overwrite :if-does-not-exist :create)
     ,@(if rest `((w/outfile ,rest ,@body))
	 body)))

(defmacro w/appendfile ((var file . rest) &body body)
  `(with-open-file (,var ,file :direction :output :if-exists :append :if-does-not-exist :create)
     ,@(if rest `((w/appendfile ,rest ,@body))
	 body)))


(defmacro w/instring ((var str . rest) &body body)
  `(with-input-from-string (,var ,str)
     ,@(if rest `((w/instring ,rest ,@body))
	 body)))

(defmacro w/outstring (vars &body body)
  `(let ,(mapcar #'(lambda (p) `(,p (outstring)))
		 vars)
     ,@body))

(defmacro fromstring (str &body body)
  `(w/instring (*standard-input* ,str)
     ,@body))

(defmacro tostring (&body body)
  `(w/outstring (*standard-output*)
     ,@body
     (inside *standard-output*)))


(defmacro w/buf ((var name . rest) &body body)
  `(let ((,var (make-buffer-stream (get-buffer-create ,name))))
     (prog1
	 (progn ,@body)
       (close ,var))))


#+xyzzy
(mapcar
 #'(lambda (l)
     (let ((i (car l))
	   (syms (cdr l)))
       (dolist (sym syms)
	 (setf (get sym 'lisp-indent-hook) i))))

 '((0 tostring)
   (1 with withs w/uniq accum afn
      w/infile w/outfile w/appendfile w/buf
      w/instring w/outstring fromstring
      repeat)
   (2 rfn whilet)
   (3 aloop for)))
