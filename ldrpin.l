;;; ldrpin command for xyzzy

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'xml-http-request)
  (require 'json)
  (unless (find-package :ldrpin)
    (defpackage :ldrpin
      (:use :lisp :editor :xhr))))


(in-package :ldrpin)
(defvar *ldr-url* "http://reader.livedoor.com/reader/")
(defvar *ldr-apiurl* "http://reader.livedoor.com/api/")
(defvar *ldr-apikey* nil)

(export '(ldrpin-add ldrpin-del ldrpin-all ldrpin-clear
          ldrpin-list ldr-getapikey
          *ldr-url* *ldr-apiurl*))


(defvar utc0 (encode-universal-time 0 0 0 1 1 1970 0))
(defun utc2ut (utc)
  (+ utc utc0))

(defun ldr-getapikey ()
  (let ((future (xhr-get-future *ldr-url* :key #'xhr-response-values :since :epoch)))
    (multiple-value-bind (res status header) (xhr-future-value future)
      (case status
        (0 (and (ldr-login)
                (ldr-getapikey)))
        (200
         (if (string-match "ApiKey = \"\\([0-9a-z]+\\)\"" res)
             (setq *ldr-apikey* (match-string 1))
             (and (msgbox "ApiKey取得失敗") nil)))
        (t (and (msgbox "接続エラー: ~A" status) nil))))))

(defun ldr-login ()
  (interactive)
  (let* ((user (ldrpin::minibuffer-input "user"))
         (pass (ldrpin::minibuffer-input "pass" t))
         (future (xhr-get-future "http://member.livedoor.com/login/index"
                                 `(:livedoor_id ,user :password ,pass)
                                 :key #'xhr:xhr-response-values
                                 :since :epoch)))
    (multiple-value-bind (res status header) (xhr-future-value future)
      (case status
        (200
          (if (string-match "認証に失敗しました" res)
              (and (msgbox "ログイン失敗") nil)
              t))
        (t (msgbox "接続エラー: ~A" status) nil)))))

(defun ldrpin-add (link &optional (title ""))
  (interactive "sURL: \nsTitle: ")
  (when (string= title "")
    (setq title link))
  (let ((res (ldr-api "pin/add" `(:link ,(map-internal-to-utf-8 link)
                                  :title ,(map-internal-to-utf-8 title)))))
    (if (string-match "\"isSuccess\":1" res)
        (message "Done (~A)" link)
        (message "Failed"))))

(defun ldrpin-del (link)
  (interactive "sURL: ")
  (let ((res (ldr-api "pin/remove" `(:link ,(map-internal-to-utf-8 link)))))
    (if (string-match "\"isSuccess\":1" res)
        (message "Removed (~A)" link)
        (message "Failed"))))

(defun ldrpin-clear ()
  (interactive)
  (let ((res (ldr-api "pin/clear")))
    (if (string-match "\"isSuccess\":1" res)
        (message "Cleared")
        (message "Failed"))))

(defun ldrpin-all ()
  (ldr-api "pin/all"))

(defun ldrpin-list ()
  (interactive)
  (set-buffer (create-new-buffer "*ldr-pin*"))
  (with-output-to-selected-buffer
    (let ((pins (reverse (json:json-decode (ldrpin-all)))))
      (dolist (item pins)
        (format t "----~%")
        (mapc #'(lambda (key)
                  (let ((p (assoc key item :test #'string=)))
                    (format t "~A~%"
                            (if (string= key "created_on")
                                (format-date-string "%y/%m/%d %H:%M:%S" (utc2ut (cdr p)))
                                (cdr p)))))
              '("title" "link" "created_on")))))
  (not-modified))

(defun ldr-api (api &optional query)
  (when (or *ldr-apikey* (ldr-getapikey))
    (multiple-value-bind (res status header)
        (xhr-post (concat *ldr-apiurl* api)
                  `(,@query :ApiKey ,*ldr-apikey*)
                  :key #'xhr-response-text
                  :since :epoch)
      res)))

(defun ldr-api-async (api query &key oncomplete)
  nil)

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

(provide "ldrpin")
