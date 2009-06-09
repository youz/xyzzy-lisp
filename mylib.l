;;; utility-functions
(defun to_i (s)
  (cond
   ((numberp s) (round s))
   (t (parse-integer s))))

(defun to_s (i)
  (format nil (if (listp i) "~S" "~A") i))

(defun puts (&rest args)
  (format t "~{~A~%~}" args))

(defun fmt (formatstring param)
  (dolist (p param)
    (insert (format nil formatstring (if (symbolp p) (symbol-name p) p)))))

(defun read-csv (str)
  (cdr (read-from-string (format nil "`(,~A)" str))))

(defun cs (&rest xs)
  (format t "~&~{~A~^,~}~%" xs))

(let ((utc0 (encode-universal-time 0 0 0 1 1 1970 0)))
  (defun ut2utc (ut)
    (- ut utc0))

  (defun utc2ut (utc)
    (+ utc utc0)))

(defun encode-utc (&rest params)
  (ut2utc (apply #'encode-universal-time params)))


(defun read-file-chars (fn &key encoding)
  (with-open-file (in fn :encoding (or encoding :text))
    (loop for i = (read-char in nil nil)
      while (peek-char nil in nil nil)
      collect i)))

(defun read-file-lines (fn)
  (with-open-file (in fn)
    (loop for l = (read-line in nil nil)
      while (peek-char nil in nil nil)
      collect l)))

(defun read-file-string (fn)
  (format nil "~{~A~%~}" (read-file-lines fn)))

(defun read-file-csv (fn)
  (mapcar #'read-csv (read-file-lines fn)))

;;; xhr
(require 'xml-http-request)
(require 'xml-parser-modoki)
(defun wget-txt (url)
  (xhr:xhr-get url
	       :key #'xhr:xhr-response-text
	       :since :epoch))

(defun wget-xml (url &key (encoding *encoding-sjis*))
  (let* ((res (xhr:xhr-get url))
	 (xml (xhr:xhr-response-xml res)))
    (if xml xml
      (xmlpm-parse-string
       (xhr:xhr-response-text res) :encoding encoding))))



;;; macros
#|
(set-macro-character #\{
  #'(lambda (s c)
      (let ((params (split-string (symbol-name (read s)) " "))
	    (body (read-delimited-list #\} s t)))
	`(lambda ,(mapcar #'(lambda (sym) (intern sym)) params) ,@body))))
|#

;; from cl-ppcre/util.lisp
(defmacro with-unique-names ((&rest bindings) &body body)
  `(let ,(mapcar
	  #'(lambda (binding)
	      (check-type binding (or cons symbol))
	      (if (consp binding)
		  (destructuring-bind (var x) binding
		    (check-type var symbol)
		    `(,var (gensym ,(format nil "~A" x))))
		`(,binding (gensym ,(symbol-name binding)))))
	  bindings)
     ,@body))

(defmacro with-rebinding (bindings &body body)
  ;; reference implementation posted to comp.lang.lisp as
  ;; <cy3wv0fya0p.fsf@ljosa.com> by Vebjorn Ljosa - see also
  ;; <http://www.cliki.net/Common%20Lisp%20Utilities>
  (loop for binding in bindings
        for var = (if (consp binding) (car binding) binding)
        for name = (gensym)
        collect `(,name ,var) into renames
        collect ``(,,var ,,name) into temps
        finally (return `(let ,renames
                          (with-unique-names ,bindings
                            `(let (,,@temps)
                              ,,@body))))))

;; from clisp
(defmacro doseq ((var seq &optional result) &body body)
  `(block nil
     (map nil (lambda (,var) ,@body) ,seq) (return ,result)))

(defmacro each ((var arr &optional res) &body body)
  (with-gensyms (l i)
    `(let ((,l (length ,arr)))
       (dotimes (,i ,l ,res)
	 (let ((,var (aref ,arr ,i)))
	   ,@body)))))

(defmacro time (f)
  `(let ((start (get-internal-real-time))
	 (result (multiple-value-list ,f)))
     (format t "~D msec~%" (- (get-internal-real-time) start))
     (values-list result)))

;; stream
(defmacro >> (fn &body body)
  `(let ((stdout *standard-output*)
	 (*standard-output*
	  (open ,fn :direction :output
		:if-exists :append
		:if-does-not-exist :create)))
     (prog1
	 (progn ,@body)
       (close *standard-output*))))

(defmacro >>str (&body body)
  `(let ((stdout *standard-output*)
	 (*standard-output* (make-string-output-stream)))
     ,@body
     (get-output-stream-string *standard-output*)))

(defmacro >>clip (&body body)
  `(copy-to-clipboard (>>str ,@body)))

(defmacro >>buf ((buf &optional point eob) &body body)
  `(let ((stdout *standard-output*)
	 (*standard-output* 
	  (cond ((bufferp ,buf) (make-buffer-stream ,buf ,point ,eob))
		((stringp ,buf) (make-buffer-stream (get-buffer-create ,buf) ,point ,eob))
		(t *standard-output*))))
     (prog1
	 (progn ,@body)
       (close *standard-output*))))

(defmacro with< ((stream file) &body body)
  `(with-open-file (,stream ,file :direction :input :if-does-not-exists :error)
     ,@body))

(defmacro with> ((stream file) &body body)
  `(with-open-file (,stream ,file :direction :output :if-exists :overwrite :if-does-not-exist :create)
     ,@body))

(defmacro with>> ((stream file) &body body)
  `(with-open-file (,stream ,file :direction :output :if-exists :append :if-does-not-exist :create)
     ,@body))

(defmacro with>>buf ((stream name) &body body)
  `(let ((,stream (make-buffer-stream (get-buffer-create ,name))))
     (prog1
	 (progn ,@body)
       (close ,sym))))

(defmacro _listen (s)
  (if (peek-char nil s nil nil)
      t
    nil))



;; macros for editor
(defmacro with-narrowing ((from to) &body body)
  `(progn
     (narrow-to-region ,from ,to)
     ,@body
     (widen)))



;;; commands
(defun bc (fname)
  (interactive "FFile: " :title0 "Byte Compile and Load")
  (when (file-exist-p fname)
    (let ((cur (directory-namestring (get-buffer-file-name (selected-buffer))))
	  (dir (directory-namestring fname)))
      (cd dir)
      (byte-compile-file fname)
      (load (concat fname "c"))
      (cd cur))))

(defun bcdir (dir)
  (interactive "DDirectory: ")
  (let ((files (directory dir :absolute t :wild "*.l")))
    (dolist (f files)
      (byte-compile-file f))))

(defun scrap ()
  (interactive)
  (set-buffer (get-buffer-create "*scrap*"))
  (lisp-interaction-mode))

(defun package-definition (name)
  (interactive "SPackage: ")
  (insert (format nil
"(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :~A)
    (defpackage :~:*~A
      (:use :lisp :editor))))

(in-package :~:*~A)

(export '(
	  ))
" name)))

(defun mac-region (from to)
  (interactive "*r")
  (let ((sexp (eval (read-from-string
		     (concat "(macroexpand '"
			     (buffer-substring from to)
			     ")")))))
    (goto-char (max from to))
    (insert (format nil "~&~S~%" sexp))))

(defun listup-functions ()
  (interactive)
  (let* ((fs (editor::lisp-build-summary-of-functions))
	 (buf (create-new-buffer "*functions*"))
	 (bs (make-buffer-stream buf)))
    (format bs "~{~{~*~A~}~%~}" fs)
    (close bs)
    (pop-to-buffer buf (floor (* (screen-width) 1/5)) t)
    (set-buffer-modified-p nil buf)))


;; directory-shortcut
(defun filename-history-dirs ()
  (delete-duplicates
   (append
    (mapcar #'directory-namestring *minibuffer-file-name-history*)
    *minibuffer-directory-name-history*)
   :test #'string-equal))

(defun find-file-from-folder-history (path)
  (interactive (list (completing-read "Folder: "
				      (filename-history-dirs)
				      :case-fold t
				      :must-match t)))
  (multiple-value-bind (files ok) (filer path t "Find file")
    (when (and ok files)
      (dolist (f files)
	(find-file f)))))


;; pathname
(defun dir (root wc)
  (interactive "Droot: \nswildcard: " :title0 "insert filelist")
  (let ((files (directory root :recursive t :wild wc)))
    (if files
	(dolist (f files)
	  (insert (merge-pathnames f root))
	  (newline))
      (insert "not found.\n"))
    files))

(defun insert-path ()
  (interactive)
  (insert (substitute #\\ #\/ (get-buffer-file-name) :test #'char=)))

(defun insert-dirname ()
  (interactive)
  (insert (substitute #\\ #\/ (directory-namestring (get-buffer-file-name)) :test #'char=)))

(defun insert-filename ()
  (interactive)
  (insert (file-namestring (get-buffer-file-name))))


;; buffer
(defun kill-buffers (bufname)
  (interactive "BKill Buffers: " :title0 "Kill Buffers")
  (if (bufferp bufname)
      (kill-buffer (bufname))
    (dolist (buf (buffer-list) t)
      (when (string-match bufname (buffer-name buf))
	(kill-buffer buf)))))

(defun toggle-buffer-eol-code ()
  (interactive)
  (set-buffer-eol-code (mod (1+ (buffer-eol-code)) 2)))

(defun toggle-read-only2 ()
  (interactive)
  (if (eq buffer-read-only t)
      (set-buffer-colors nil)
    (set-buffer-colors #(0 #xcccccc)))
  (toggle-read-only))

(defun grep-buffer (pattern)
  (interactive "sGrep: \np" :default0 *last-grep-regexp* :history0 'search)
  (grep pattern t))

(defun comment-region (begin end)
  (interactive "*r")
  (if (< end begin) (rotatef begin end))
  (with-narrowing (begin end)
    (goto-char (point-min))
    (loop
      (goto-bol)
      (insert comment-start)
      (unless (forward-line 1) (return)))))

(defun uncomment-region (begin end)
  (interactive "*r")
  (if (< end begin) (rotatef begin end))
  (with-narrowing (begin end)
    (goto-char (point-min))
    (replace-buffer "^ *;+ ?" "" :regexp t)))

(defun copy-sexp-to-clipboard ()
  (interactive)
  (save-excursion
    (let ((p (point))(m (mark t)))
      (when (string/= "(" (buffer-substring p (1+ p)))
	(backward-sexp))
      (mark-sexp)
      (copy-region-to-clipboard (point) (mark))
      (when m (set-mark m)))))


(require 'calc)
(defun aexp-to-sexp (expr)
  (interactive "sExpression: ")
  (with-input-from-string (s expr)
    (do ((token nil)
	 (tok (ed::calc-next-token s) (ed::calc-next-token s)))
	((null tok) (let ((ed::*calc-token* (nreverse token)))
		      (insert (format nil "~A" (ed::calc-expr)))))
      (push tok token))))

(defun region-aexp-to-sexp (start end)
  (interactive "r")
  (aexp-to-sexp (buffer-substring (region-beginning) (region-end))))


(defun palindromize-region (begin end)
  (interactive "*r")
  (insert (reverse (buffer-substring (point) (mark)))))

(defun start-selection-as-block ()
  (interactive)
  (start-selection 3 nil))


;; datetime
(defun today ()
  (interactive)
  (insert (format-date-string "%Y-%m-%d %a.")))

(defun now ()
  (interactive)
  (insert (format-date-string "%Y-%m-%d %H:%M:%S")))


;; golf
(defun ag-statistics ()
  (interactive)
  (let ((bin 0) (alnum 0) (sym 0) (ws 0))
    (doseq (c (buffer-substring (point-min) (point-max))
	      (message "~A/~A/~A (ws:~A)" bin alnum sym ws))
      (let ((cs (string c))
	    (cn (char-code c)))
	(cond
	 ((string-match "[a-zA-Z0-9]" cs)
	  (incf alnum))
	 ((string-match "[ \t\n]" cs)
	  (incf ws))
	 ((not (<= 32 cn 127))
	  (incf bin))
	 (t (incf sym)))))))



;; eshell
(defun exec-eshell (&optional name)
  (interactive)
  (unless name
    (setq name (caar erepl-programs)))
  (let ((*eshell* (cdr (assoc name erepl-programs :key #'symbol-name :test #'string=))))
    (unless *eshell*
      (return-from exec-eshell nil))
    (set-buffer (get-buffer-create (format nil "*~A*" name)))
    (let ((proc (buffer-process (selected-buffer))))
      (and proc (eq (process-status proc) ':run)
	   (return-from exec-eshell t)))
    (goto-char (point-max))
    (ed::shell-mode)
    (make-process *eshell* :output (selected-buffer))))

(defparameter eshell-programs
  '((sbcl . "sbcl.bat")
    (arc . "arc.bat --no-rl")
    (gosh . "c:/usr/Gauche/bin/gosh.exe -i")
    (ypsilon . "ypsilon.bat")
    (mosh . "mosh.bat")
    (clojure . "clj.bat --nojline")
    (v8 . "v8sh.exe")
    (sh . "C:/Msys/1.0.11/bin/sh.exe --login -i")
    (cyan . "Cyan.bat")))

(dolist (pair eshell-programs)
  (setf (symbol-function (car pair))
	`(lambda () (interactive) (exec-eshell (symbol-name ',(car pair))))))


(defun view-register-list-key ()
  (interactive)
  (with-output-to-temp-buffer ("*register-list*")
    (dotimes (i 255)
      (let* ((r (code-char i))
	     (val (ed::get-register r)))
	(when  val
	  (format t "~3@A: " r)
	  (cond ((markerp val)
		 (let ((buf (marker-buffer val)))
		   (if buf
		       (format t "pos : buffer ~a, position ~a~%"
			       (buffer-name buf) (marker-point val))
		     (princ "pos - a marker in deleted buffer."))))
		((and (consp val)
		      (eq (car val) 'window-configuration))
		 (princ "winconf (~{~A ~}~)%") (cdr val))
		((consp val)
		 (format t "rect: ~a...~%n" (car val)))
		((stringp val)
		 (format t "text: ~a~%" (car (split-string val #\LFD))))
		(t
		 (format t "garbage: ~s~%" val)))))))
  (pop-to-buffer "*register-list*" t)
  (other-window))




(defun set-lisp-indent-hook (al)
  (dolist (l al)
    (let ((i (car l))
	  (syms (cdr l)))
      (dolist (sym syms)
	(setf (get sym 'lisp-indent-hook) i)))))

(set-lisp-indent-hook
 '((0 >>str >>clip)
   (1 with-unique-names with-rebinding doseq each with-narrowing
      >> >>bf with< with> with>> with>>buf)
   (2 set-dispatch-macro-character)))
