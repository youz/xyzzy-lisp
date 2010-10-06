(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "cairo"))

(defpackage :cairo-ut
  (:use :lisp :editor :cairo :cairo.ext))

(in-package :cairo-ut)

(export '(draw-to-png
	  hsv-to-rgb
	  ))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym))) syms)
     ,@body))

;; color
(defun hsv-to-rgb (h s v)
  (let* ((hi (mod (floor h 60) 6))
	 (f (- (/ (mod h 360) 60d0) hi))
	 (p (* v (- 1 s)))
	 (q (* v (- 1 (* f s))))
	 (u (* v (- 1 (* (- 1 f) s)))))
    (case hi
      (0 (list v u p))
      (1 (list q v p))
      (2 (list p v u))
      (3 (list p q v))
      (4 (list u p v))
      (5 (list v p q)))))

;;macros

(defmacro draw-to-png ((cr output-filename
		      &key (height 256) (width 256)
		      (base :white)
		      (normalize t)
		      (time nil))
		  &body body)
  (let ((base-color (case base
		      (:black '(0 0 0))
		      (:blue '(0 0 1))
		      (:green '(0 1 0))
		      (:dark-blue '(0 0 0.5))
		      (:dark-green '(0 0.5 0))
		      (t '(1 1 1)))))
    (with-gensyms (surface start elapsed fnt)
      `(progn
	 (delete-file ,output-filename :if-does-not-exist :skip :if-access-denied :skip)
	 (let ((,start (get-internal-real-time)))
	   (with-cairo-surface (,surface (cairo-image-surface-create :argb32 ,width (+ ,height (if ,time 15 0))))
	     (with-cairo (,cr (cairo-create ,surface))
	       (with-output-to-png (,surface ,output-filename)
		 (with-cairo-save (,cr)
		   (when ,normalize
		     (cairo-scale ,cr ,width ,height))
		   (with-cairo-save (,cr)
		     (cairo-set-source-rgb ,cr ,@base-color)
		     (cairo-paint ,cr))
		   (progn ,@body))
		 ;; Elapsed Time
		 (when ,time
		   (let ((,elapsed (- (get-internal-real-time) ,start)))
		     (with-cairo-save (,cr)
		       (cairo-set-source-rgba ,cr 0 0 0 0.8)
		       (let ((,fnt (cairo-win32-font-face-create "Courier")))
			 (cairo-set-font-face ,cr ,fnt))
		       (cairo-set-font-size ,cr 12)
		       (cairo-move-to ,cr 3 (+ ,height 11))
		       (cairo-show-text ,cr (format nil "time: ~,2F ms"
						    (/ ,elapsed 1000.0))))))))))))))

(setf (get 'draw-to-png #1='ed::lisp-indent-hook) 1
      (get 'with-cairo #1#) 1
      (get 'with-cairo-save #1#) 1
      (get 'with-cairo-surface #1#) 1
      (get 'with-output-to-png #1#) 1)


#|
(defun delicious? (&optional (l 256))
  (draw-to-png (cr (concat (user::pwd) "xor.png")
	      :normalize nil :width l :height l :time nil)
    (cairo-set-antialias cr :none)
    (dotimes (y l)
      (cairo-move-to cr 0 y)
      (dotimes (x l)
	(let* ((r (/ (logxor x y) l))
	       (g r)
	       (b (/ (logior x y) l)))
	  (cairo-set-source-rgb cr r g b)
	  (with-cairo-fill (cr)
	    (cairo-rectangle cr x y 1 1)
	    ))))))

(delicious? 512)
|#

(provide "cairo-ut")
