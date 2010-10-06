;;; goo.gl url shortener interface for xyzzy

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'cmu_loop)
  (require 'xml-http-request)
  (require 'json)
  (unless (find-package :googl)
    (defpackage :googl
      (:use :lisp :editor))))

(in-package :googl)
(export '(shorten))

(defun _c (&rest xs)
  (loop
    for x in xs
    sum (logand x 4294967295)))

(defun _d (l)
  (let* ((ls (format nil "~A" (if (<= l 0) (+ l 4294967296) l)))
	 (o 0))
    (loop
      for c across (reverse ls)
      for q = (- (char-code c) 48)
      for n = nil then (not n)
      do (incf o (if n (+ (floor #1=(* q 2) 10) (mod #1# 10)) q)))
    (let* ((m (- 10 (mod o 10)))
	   (d (cond ((= m 10) 0)
		    ((evenp (length ls)) m)
		    ((oddp m) (/ (+ m 9) 2))
		    (t (/ m 2)))))
      (format nil "~A~A" d ls))))

(defun _e (uri)
  (loop
    for c across uri
    for m = 5381
    then (_c (ash m 5) m (char-code c))
    finally (return m)))

(defun _f (uri)
  (loop
    for c across uri
    for m = 0
    then (_c (char-code c) (ash m 6) (ash m 16) (* m -1))
    finally (return m)))

(defun make-auth-token (uri)
  (flet (($ (n d u) (ash (logand (ash n (- d)) 15) u)))
    (let* ((i (_e uri))
	   (h (_f uri))
	   k)
      (setq i (logand (ash i -2) 1073741823)
	    i (logior (logand #1=(ash i -4) 67108800) (logand i 63))
	    i (logior (logand #1# 4193280) (logand i 1023))
	    i (logior (logand #1#  245760) (logand i 16383))
	    k (logior ($ i 2 4) ($ h 0 0)
		      ($ i 6 12) ($ h 8 8)
		      ($ i 10 20) ($ h 16 16)
		      ($ i 14 28) ($ h 24 24)))
      (format nil "7~A" (_d k)))))

(defun shorten (uri &optional (user "toolbar@google.com"))
  (let* ((data `(:user ,user :url ,uri :auth_token ,(make-auth-token uri)))
	 (res (xhr:xhr-post "http://goo.gl/api/url" data
			    :key #'xhr:xhr-response-text)))
    (when res
      (cdr (assoc "short_url" (json:json-decode res) :test 'equal)))))

(provide "googl")
