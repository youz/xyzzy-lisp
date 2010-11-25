;;; -*- mode: lisp; package: ldrpin -*-

;;; ldrpin mode for xyzzy

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'xml-http-request)
  (require 'json)
  (require 'www/www)
  (require "expand-short-url")
  (require "googl")
  (require "hatebu"))

(defpackage :ldrpin
  (:use :lisp :editor :xhr))


(in-package :ldrpin)
(defvar *ldr-url* "http://reader.livedoor.com/reader/")
(defvar *ldr-apiurl* "http://reader.livedoor.com/api/")
(defvar *ldr-apikey* nil)
(defvar *ldr-username* nil)
(defvar *ldr-password* nil)
(defvar *use-frame* t)
(defvar *frame-name* "LDR Pins")

(defparameter *adfeed-urls*
  '("pheedo\\.jp" "rssad\\.jp" "feedbunner" "feeds" "feedproxy.google.com"))

(defvar *ldrpin-buffer-name* "*ldr-pins*")
(defvar *ldrpin-mode-map* nil)
(defvar ldr-pins nil)


(export '(ldrpin-add ldrpin-del ldrpin-all ldrpin-clear
	  ldrpin-list ldr-getapikey
	  *ldr-url* *ldr-apiurl*
	  *ldr-username* *ldr-password*
	  *ldrpin-mode-map*))


;;; utilities
(defmacro whenlet (var expr &body body)
  `(let ((,var ,expr)) (when ,var ,@body)))

(defmacro json-value (obj key)
  `(cdr (assoc ,(symbol-name key) ,obj :test #'string=)))

(defmacro w/json (keys obj &body body)
  (let ((gobj (gensym)))
    `(let* ((,gobj ,obj)
	    ,@(mapcar #'(lambda (k) `(,k (json-value ,gobj ,k))) keys))
       ,@body)))

(defun entity2char (str)
  (reduce #'(lambda (s pair) (apply #'substitute-string s pair))
	  '(("&amp;" "&") ("&gt;" ">") ("&lt;" "<") ("&quot;" "\""))
	  :initial-value str))

(defun utc2ut (utc)
  (+ utc 2208988800))

(defun format-datetime (utc)
  (format-date-string
   "%y/%m/%d %H:%M:%S" (utc2ut utc)))

(defun print-elm (contents tag &rest attrs)
  (let ((start (point-max)))
    (format t "~A~%" contents)
    (apply #'set-text-attribute start (1- (point-max)) tag attrs))
  contents)

(defun expand-url (url)
  (if (some #'(lambda (u) (string-match u url)) *adfeed-urls*)
      (substitute-string (user:expand-short-url url t) "\\?ref=rss" "")
    url))


;;; ldr api
(defun ldr-getapikey ()
  (let ((future (xhr-get-future *ldr-url* :key #'xhr-response-values :since :epoch)))
    (multiple-value-bind (res status header) (xhr-future-value future)
      (case status
        (0 (and (ldr-login)
                (ldr-getapikey)))
        (200
         (if (string-match "ApiKey = \"\\([0-9a-z]+\\)\"" res)
             (setq *ldr-apikey* (match-string 1))
             (and (msgbox "ApiKeyæ“¾¸”s") nil)))
        (t (and (msgbox "Ú‘±ƒGƒ‰[: ~A" status) nil))))))

(defun ldr-login ()
  (interactive)
  (let* ((username (or *ldr-username* (minibuffer-input "user")))
	 (password (or *ldr-password* (minibuffer-input "pass" t)))
	 (future (xhr-post-future "http://member.livedoor.com/login/index"
                                 `(:livedoor_id ,username :password ,password)
                                 :key #'xhr:xhr-response-values
                                 :since :epoch)))
    (multiple-value-bind (res status header) (xhr-future-value future)
      (case status
        (200
         (if (string-match "”FØ‚É¸”s‚µ‚Ü‚µ‚½" res)
             (and (msgbox "ƒƒOƒCƒ“¸”s") nil)
             t))
        (t (msgbox "Ú‘±ƒGƒ‰[: ~A" status) nil)))))

(defun ldr-api (api &optional query ok retry)
  (when (or *ldr-apikey* (ldr-getapikey))
    (handler-case
	(multiple-value-bind (res status header)
	    (xhr-post (concat *ldr-apiurl* api)
		      `(,@query :ApiKey ,*ldr-apikey*)
		      :key #'xhr-response-text
		      :since :epoch)
	  (if ok
	      (if (string-match "\"isSuccess\":1" res) t nil)
	    res))
      (xml-http-request:xhr-open-error (c)
	(unless retry
	  (ldr-getapikey)
	  (ldr-api api query ok t))))))

(defun ldr-api-async (api query &key oncomplete)
  nil)


(defun ldrpin-add (link &optional (title ""))
  (interactive "sURL: \nsTitle: ")
  (when (string-match "?utm_source" link)
    (setf link (subseq link 0 (match-beginning 0))))
  (let* ((title (or title link))
	 (ok (ldr-api "pin/add" `(:link ,(map-internal-to-utf-8 link)
				  :title ,(map-internal-to-utf-8 title))
		      t)))
    (message "~:[Failed~;Done (~A)~]" ok link)
    ok))

(defun ldrpin-del (link)
  (interactive "sURL: ")
  (let* ((ok (ldr-api "pin/remove" `(:link ,(map-internal-to-utf-8 link)) t)))
    (message "~:[Failed~;Removed (~A)~]" ok link)
    ok))

(defun ldrpin-clear ()
  (interactive)
  (let* ((ok (ldr-api "pin/clear") t))
    (message "~:[Failed~;Cleared~]" ok)
    ok))

(defun ldrpin-all ()
  (nreverse (json:json-decode (ldr-api "pin/all"))))

(defun ldrpin-list ()
  (save-excursion
    (with-output-to-selected-buffer
      (let ((pins (ldrpin-all))
	    (n 0))
	;(rename-buffer (format nil "~A (~D)" *ldrpin-buffer-name* (length pins)))
	(format t "~D pins~%" (length pins))
	(dolist (item pins)
	  (w/json (link title created_on) item
	    (let ((title (entity2char title))
		  (link (entity2char link)))
	      (setf (gethash link ldr-pins) t)
	      (print-elm (format nil "[~2,'0D]~75@{-~}" (incf n) t)
			 :entry :foreground 14 :bold t)
	      (print-elm title :title)
	      (print-elm link :link)
	      (print-elm (format-datetime created_on) :date))))
	(message "~D pins loaded." (length pins))))
    (not-modified)))



(defun minibuffer-input (prompt &optional (pass nil))
  (let ((in (make-vector 16 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop
      (if pass
          (minibuffer-prompt "~A: ~v@{~a~:*~}" prompt (length in) #\*)
          (minibuffer-prompt "~A: ~A" prompt in))
      (let ((c (read-char *keyboard*)))
        (case c
          (#\RET
           (return in))
          (#\C-g
           (quit))
          (#\C-q
           (vector-push-extend (read-char *keyboard*) in))
          (#\C-h
           (or (zerop (length in))
               (vector-pop in)))
          (t
           (vector-push-extend c in)))))))


;;; ldrpin-list-mode

(defun current-item (tag)
  (save-excursion
    (whenlet entry-start (find-text-attribute :entry :end (1+ (point)) :from-end t)
      (goto-char entry-start)
      (multiple-value-bind (start end tag)
	  (find-text-attribute tag :start (point))
	(if start
	    (values (buffer-substring start end) start end))))))

;;; ldrpin list mode

(defun next-entry ()
  (interactive)
  (whenlet start (find-text-attribute :entry :start (1+ (point)))
    (goto-char start)
    (forward-line 2)
    (recenter)))

(defun previouse-entry ()
  (interactive)
  (goto-bol)
  (whenlet start (find-text-attribute :entry :end (point) :from-end t)
    (goto-char start)
    (forward-line -2)
    (recenter)))

(defun toggle-pin ()
  (interactive)
  (let ((link (current-item :link))
	(title (current-item :title)))
    (when link
      (multiple-value-bind (s start end) (current-item :entry)
	(if #1=(gethash link ldr-pins)
	  (and (ldrpin-del link)
	       (set-text-attribute start end :entry :foreground 15)
	       (setf #1# nil))
	  (and (setq link (expand-url link))
	       (ldrpin-add link title)
	       (set-text-attribute start end :entry :foreground 14 :bold t)
	       (setf #1# t)))))))

(defun open-current-link ()
  (interactive)
  (whenlet link (current-item :link)
    (user::www-open-url link)))

(defun open-current-link-in-browser ()
  (interactive)
  (whenlet link (current-item :link)
    (shell-execute link t)))

(defun copy-current-link ()
  (interactive)
  (whenlet link (current-item :link)
    (copy-to-clipboard link)
    (message "copied: ~A" link)))

(defun copy-current-link-as-short-url ()
  (interactive)
  (let* ((link (current-item :link))
	 (short (googl:shorten link)))
    (when short
      (copy-to-clipboard short)
      (message "copied: ~A (~A)" short link))))
      
(defun ldrpin-quit ()
  (interactive)
  (kill-buffer (selected-buffer))
  (whenlet fr (and *use-frame* (find-pseudo-frame *frame-name*))
    (delete-pseudo-frame fr)))


;;; showing hatena bookmark

(defun show-hatebu-comments ()
  (interactive)
  (whenlet url (current-item :link)
    (hatebu:show-comments url)))



(setq *ldrpin-mode-map* (make-sparse-keymap))
(dolist (b '((#\p toggle-pin)
	     (#\j next-entry)
	     (#\k previouse-entry)
	     (#\v open-current-link)
	     (#\o open-current-link-in-browser)
	     (#\c copy-current-link)
	     (#\C copy-current-link-as-short-url)
	     (#\q ldrpin-quit)
	     (#\b show-hatebu-comments)
	     (#\r user::ldrpin)))
  (apply #'define-key *ldrpin-mode-map* b))


(defun ldrpin-mode ()
  (kill-all-local-variables)
  (make-local-variable 'indent-tabs-mode)
  (setq mode-name "LDR-Pins"
	indent-tabs-mode nil
	buffer-read-only t
	need-not-save t
	kept-undo-information nil
	auto-save nil)
  (toggle-ime nil)
  (set-local-window-flags (selected-buffer)
			  *window-flag-line-number* nil)
  (set-local-window-flags (selected-buffer)
			  *window-flag-newline* nil)
  (set-local-window-flags (selected-buffer)
			  *window-flag-eof* nil)
  (set-local-window-flags (selected-buffer)
			  *window-flag-cursor-line* t)
  (set-buffer-fold-width t)
;  (make-local-variable 'regexp-keyword-list)
;  (setq regexp-keyword-list *ldrpin-regexp-keyword-list*)
  (use-keymap *ldrpin-mode-map*))


(defun user::ldrpin ()
  (interactive)
  (when *use-frame*
    (select-pseudo-frame (or (find-pseudo-frame *frame-name*)
			     (new-pseudo-frame *frame-name*))))
  (set-buffer (get-buffer-create *ldrpin-buffer-name*))
  (setq buffer-read-only nil)
  (erase-buffer (selected-buffer))
  (setq ldr-pins (make-hash-table :test #'equal))
  (ldrpin-list)
  (ldrpin-mode))

(defun user::ldrpin-add-region (start end title)
  (interactive "r\nsTitle: " :default1 (buffer-name (selected-buffer)))
  (let ((url (buffer-substring start end)))
    (ldrpin-add url title)))

(provide "ldrpin")
