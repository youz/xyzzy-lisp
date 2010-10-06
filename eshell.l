(require "shell3")
(in-package "editor")

(export '(exec-eshell *eshell-profiles*))

(defparameter *eshell-profiles*
  `((:name "sbcl" :com "sbcl.bat" :prompt "^\\* ")
    (:name "clisp" :com "clisp.bat" :prompt "^[\\[0-9\\]+]> ")
    (:name "gosh" :com "gosh.exe -i" :encoding ,*encoding-utf8n*)
    (:name "ypsilon" :com "ypsilon.bat")
    (:name "arc" :com "arc.bat")
    (:name "clojure" :com "clj.bat --nojline" :prompt "^[a-zA-Z./\\-]+=> ")
    (:name "sh" :com "C:/Msys/1.0.11/bin/sh.exe --login -i")
    (:name "cyan" :com "Cyan.bat")))

(defun exec-eshell (name)
  (interactive
      (list (completing-read "eshell: "
                             (mapcar #'cadr *eshell-profiles*)
                             :case-fold t
                             :must-match t)))
  (let* ((prf (find name *eshell-profiles* :key #'cadr :test #'string=))
         (*eshell* (getf prf :com))
         (prompt (or (getf prf :prompt) "^[^#$%>?\n]*[#$%>?] ?"))
         (encoding (or (getf prf :encoding) *encoding-sjis*)))
    (unless *eshell*
      (return-from exec-eshell nil))
    (set-buffer (get-buffer-create (format nil "*~A*" name)))
    (change-fileio-encoding encoding)
    (make-local-variable 'shell-prompt-regexp2)
    (setq shell-prompt-regexp2 prompt)
    (let ((proc (buffer-process (selected-buffer))))
      (and proc (eq (process-status proc) ':run)
           (return-from exec-eshell t)))
    (goto-char (point-max))
    (ed::shell-mode)
    (make-process *eshell*
                  :output (selected-buffer)
                  :incode encoding
                  :outcode encoding)))

(in-package "user")
(dolist (p *eshell-profiles*)
  (let ((sym (intern (cadr p))))
    (setf (symbol-function sym)
          `(lambda () (interactive) (exec-eshell ,(cadr p))))))
