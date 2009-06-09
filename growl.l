;;; growl.l --- Growl command for xyzzy

;;; Growl for Windows  http://www.growlforwindows.com/gfw/default.aspx
;;; GNTP specification http://www.growlforwindows.com/gfw/help/gntp.aspx

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "growl")
    (defpackage "growl"
      (:use "lisp" "editor"))))

(provide "growl")

(in-package "growl")
(export '(growl-notify
	  growl-register
	  *growl-default-name*
	  *growl-default-icon*))

(defvar *growl-default-icon*
  (merge-pathnames "etc/xyzzy-wiki.png" (si:system-root)))

(defvar *growl-default-name*
  "xyzzy-default")


(defun growl-notify (title text &key name icon)
  (growl-register (or name *growl-default-name*))
  (growl-send-messages
   `("GNTP/1.0 NOTIFY NONE"
     "Application-Name: xyzzy"
     ,(format nil "Notification-Name: ~A" (or name "xyzzy-default"))
     ,(format nil "Notification-Title: ~A" title)
     ,(format nil "Notification-Text: ~A" text)
     ,(format nil "Notification-Icon: ~A" (or icon *growl-default-icon*))
     )))


(defun growl-register (name)
  (growl-send-messages
   `("GNTP/1.0 REGISTER NONE"
     "Application-Name: xyzzy"
     "Notifications-Count: 1"
     ""
     ,(format nil "Notification-Name: ~A" name)
     ,(format nil "Notification-Display-Name: ~A" name)
     "Notification-Enabled: True"
     )))


(defun growl-send-messages (messages)
  (with-open-stream (stream (connect "127.0.0.1" 23053 :encoding :binary))
    (dolist (msg messages)
      (format stream "~A\r\n" (convert-encoding-from-internal *encoding-utf8n* msg)))
    (princ "\r\n" stream)
    (let ((result (read-line stream nil)))
      (if (string/= result "GNTP/1.0 -OK NONE \r")
	  (and (message-box result) nil)
	t))))


(in-package "user")

(defun growl (title text)
  (interactive "sTitle: \nsText: ")
  (growl::growl-notify title text))

(let ((state nil)
      (msgo #'message)
      (msgg #'(lambda (&rest args) (growl "---" (apply #'format nil args)))))
  (defun toggle-growl ()
    (interactive)
    (setf (symbol-function 'message)
	  (if (setq state (null state)) msgg msgo))))
