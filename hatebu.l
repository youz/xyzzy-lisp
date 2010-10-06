(require "json")
(require "xml-http-request")

(defpackage :hatebu
  (:use :lisp :editor))

(in-package :hatebu)

(export '(show-comments))

(defvar *hatebu-window-ratio* 1/3)

(defvar *hatebu-mode-map* (make-sparse-keymap))
(define-key *hatebu-mode-map* #\j 'next-line)
(define-key *hatebu-mode-map* #\k 'previous-line)
(define-key *hatebu-mode-map* #\q 'quit-hatebu)

(defmacro json-value (obj key)
  (reduce #'(lambda (o k) `(cdr (assoc ,k ,o :test #'string=)))
	  (split-string (symbol-name key) #\.)
	  :initial-value obj))

(defmacro w/json (keys obj &body body)
  (let ((gobj (gensym)))
    `(let* ((,gobj ,obj)
	    ,@(mapcar #'(lambda (k) `(,k (json-value ,gobj ,k))) keys))
       ,@body)))

(defun quit-hatebu ()
  (interactive)
  (delete-buffer (selected-buffer))
  (delete-window))

(defun show-comments (url)
  (multiple-value-bind (res status)
      (xhr:xhr-future-value
       (xhr:xhr-get-future "http://b.hatena.ne.jp/entry/jsonlite/"
			   :query `(:url ,url)
			   :key #'xhr:xhr-response-values)
       :timeout 5)
    (unless status
      (error "timeout" 'simple-error))
    (if (= status 200)
	(let ((buf (get-buffer-create (format nil "*comments on ~A*" url))))
	  (with-output-to-buffer (buf)
	    (if (string= res "null")
		(format t "No bookmarks on ~A" url)
	      (w/json (count bookmarks title) (json:json-decode res)
		(format t "~A  - ~A user~%~%" title count)
		(dolist (e bookmarks)
		  (w/json (user tags comment) e
		    (format t "~&~A: ~{[~A]~}~:[~%  ~A~;~]" user tags (string= "" comment) comment))))))
	  (pop-to-buffer buf (floor (* -1 (screen-width) *hatebu-window-ratio*)) t)
	  (hatebu-mode))
      (error (format nil "~S" header) 'simple-error))))

(defun hatebu-mode ()
  (kill-all-local-variables)
  (make-local-variable 'indent-tabs-mode)
  (setq mode-name "hatebu"
	indent-tabs-mode nil
	buffer-read-only t
	need-not-save t
	kept-undo-information nil
	auto-save nil)
  (set-buffer-modified-p nil)
  (set-buffer-fold-type-window)
  (toggle-ime nil)
  (set-local-window-flags (selected-buffer)
			  *window-flag-line-number* nil)
  (set-local-window-flags (selected-buffer)
			  *window-flag-newline* nil)
  (set-local-window-flags (selected-buffer)
			  *window-flag-eof* nil)
  (set-local-window-flags (selected-buffer)
			  *window-flag-cursor-line* t)
  (use-keymap *hatebu-mode-map*))
