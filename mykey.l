(require "my/mylib")

;;; complete+
(dolist (keymap (list minibuffer-local-completion-map
		      minibuffer-local-must-match-map
		      minibuffer-local-command-line-map))
  (define-key keymap '#\C-n  'complete+-select-next-item)
  (define-key keymap '#\C-p  'complete+-select-prev-item)
  (define-key keymap '#\C-<  'complete+-substring-match-rotate)
  (define-key keymap '#\C->  'complete+-skip-match-rotate)
  (define-key keymap '#\C-\, 'complete+-case-fold-ratate)
  (define-key keymap '#\C-.  'complete+-toggle-incremental))

(setf *complete+-and-search* ";")
(setf *complete+-show-drive* t)
(setf *complete+-current-item-attribute* '(:foreground 1))
(setf *complete+-highlight-color*        '(:foreground 2))
(setf *virtual-file-add-slash-automatically* t)
(setf *complete+-create-new-file-check* t)
(complete+-toggle-incremental t)


;;;  global keymap

(defmacro defky (key com)
  `(global-set-key ',key ,(if (atom com) `',com `(lambda () (interactive) ,com))))

(defky (#\C-c #\C-m) execute-extended-command)

;window
(let ((alpha 255))
  (defun set-alpha (a)
    (layered-window-set-alpha (setq alpha (max 64 (min 255 a)))))
  
  (defky #\C-7 (set-alpha (- alpha 32)))
  (defky #\C-8 (set-alpha (+ alpha 32))))

(defky (#\C-t #\C-t) (layered-window-set-alpha 192))
(defky (#\C-t #\C-o) layered-window-set-opaque)
(defky (#\C-x #\C-m) ttray-hide-xyzzy)
(defky (#\C-x #\C-c) kill-xyzzy)
(defky #\M-* (set-buffer (find-buffer "*scratch*")))
(defky (#\C-x #\3) split-window-vertically)
(defky #\C-M-8 chcolor)

;; file
(defky #\C-] find-file-from-folder-history)

;buffer
(defky #\C-0 next-buffer)
(defky #\C-9 previous-buffer)
(defky #\M-k (kill-buffer (selected-buffer)))
(defky #\M-C-k kill-buffers)
(defky (#\C-x #\u) new-file)
(defky (#\C-x #\U) switch-to-buffer)

;frame
(defky #\M-0 next-pseudo-frame)
(defky #\M-9 previous-pseudo-frame)

;cursor
(defky #\C-M-j (scroll-window 2))
(defky #\C-M-k (scroll-window -2))
(defky #\C-M-n (scroll-window 2))
(defky #\C-M-p (scroll-window -2))
(defky #\C-f forward-char)
(defky #\Home beginning-of-line)
(defky #\C-Home beginning-of-buffer)
(defky #\End end-of-line)
(defky #\C-End end-of-buffer)
(defky #\M-C-/ beginning-of-buffer)
(defky #\M-C-\\ end-of-buffer)
(defky #\C-Right forward-word)
(defky #\C-Left backward-word)
(defky #\C-\, previous-page)
(defky #\C-. next-page)
(defky #\C-> forward-word)
(defky #\C-< backward-word)
(defky #\M-j goto-line)
(defky #\C-PageUp (scroll-down-other-window 2))
(defky #\C-PageDown (scroll-down-other-window -2))
;(defky #\C-? (scroll-down-other-window 1))
;(defky #\C-_ (scroll-down-other-window -1))

;region
(defky (#\C-k #\h) mark-whole-buffer)
(defky (#\C-k #\c) copy-region-as-kill)
(defky (#\C-k #\k) kill-region)
(defky (#\C-k #\l) kill-line)
(defky (#\C-k #\C-d) delete-region)
(defky (#\C-k #\Tab) indent-region)
(defky (#\C-k #\C-k) kill-region-to-clipboard)
(defky (#\C-k #\C-c) copy-region-to-clipboard)
(defky (#\C-k #\C-s) copy-sexp-to-clipboard)
(defky #\C-v paste-from-clipboard)

(defky (#\C-c #\/) ed::minibuffer-convert-backslash-to-slash-region)
(defky (#\C-c #\\) ed::minibuffer-convert-slash-to-backslash-region)

;selection
(defky (#\C-l #\b) start-selection-as-block) 
(defky (#\C-l #\l) start-selection-as-line)
(defky (#\C-l #\j) start-selection-as-region)
(defky (#\C-l #\a) selection-whole-buffer)
(defky #\S-Home selection-beginning-of-line)
(defky #\S-End selection-end-of-line)
(defky #\S-C-Home selection-beginning-of-buffer)
(defky #\S-C-End selection-end-of-buffer)
(defky #\M-B selection-backward-word)
(defky #\M-F selection-forward-word)

;undo-redo
(defky #\C-z undo)
(defky #\M-C-z redo)
(defky #\C-/ undo)
;(defky #\C-_ redo)

;toggle-folding
(defky (#\M-f #\z) set-buffer-fold-type-none)
(defky (#\M-f #\c) set-buffer-fold-type-column)
(defky (#\M-f #\x) set-buffer-fold-type-window)

;;search
;(defky #\M-C-f search-dialog)
(defky #\M-C-g grep-buffer)
(defky (#\C-c #\g #\g) ggrep-dialog)
(defky (#\C-c #\g #\s) gresreg-dialog)

;;replace
(defky #\M-r replace-dialog)

;;help
(defky (#\M-h #\a) command-apropos)
(defky (#\M-h #\b) describe-bindings)
(defky (#\M-h #\c) describe-key-briefly)

;;customize
(defky (#\M-i #\M-i) (find-file "~/site-lisp/siteinit.l"))
(defky (#\M-i #\M-y) (find-file "~/.xyzzy"))
(defky (#\M-i #\M-k) (find-file "~/site-lisp/my/mykey.l"))
(defky (#\M-i #\M-l) (find-file "~/site-lisp/my/mylib.l"))
(defky (#\M-i #\M-u) (find-file "~/site-lisp/cl/util.l"))

(defky (#\C-c #\M-j) (toggle-buffer-eol-code))


;;; revert-buffer
(defky (#\C-c #\C-v #\C-r) (revert-buffer (read-char-encoding "Encoding: ")))

;;; force encoding
(defky (#\C-c #\C-v #\C-f) (change-fileio-encoding (read-char-encoding "Encoding: ")))

(defky (#\C-x #\q) toggle-read-only2)


;;; ckw
(defky (#\C-c #\k #\c) (find-file "c:/usr/bin/ckw/ckwp.cfg"))

(defun run-ckw (&optional (exe nil) &key (dir nil) (title nil))
  (launch-application
   (concat (map-slash-to-backslash "c:/usr/bin/ckw/ckwp.exe")
           " -cd \""
           (cond
            (dir dir)
            ((setq fn (get-buffer-file-name)) (directory-namestring fn))
            (t (si:system-root))) "\""
           " -tl \"[" (or title "cmd") "]\""
           " -e " (or exe *eshell* *shell*))))

(macrolet ((ckw (&optional path title dir)
     `(run-ckw ,path :title ,title :dir ,dir)))
  (defky (#\C-c #\k #\w) (ckw))
  (defky (#\C-c #\k #\r) (ckw "irb.bat" "ruby"))
  (defky (#\C-c #\k #\p) (ckw "powershell.exe" "Power Shell"))
  (defky (#\C-c #\k #\s) (ckw "runsbcl.bat" "sbcl"))
  (defky (#\C-c #\k #\a) (ckw "arc.bat" "arc"))
  (defky (#\C-c #\k #\g) (ckw "gosh.bat" "Gauche"))
  (defky (#\C-c #\k #\j) (ckw "clj.bat" "Clojure"))
  (defky (#\C-c #\k #\x) (ckw "runxpc.exe" "XULRunner"))
  (defky (#\C-c #\k #\v) (ckw "v8sh.exe" "V8"))
  (defky (#\C-c #\k #\y) (ckw "ypsilon.bat" "Ypsilon"))
)


(defky (#\C-l #\C-l) (insert (code-char 33737)))
(defky (#\C-c #\w) (replace-buffer "[^wWv]" "" :regexp t))
(defky (#\C-x #\r #\l) view-register-list-key)

;;compilie site-init.l & delete dump
(defky (#\C-c #\b) compile-and-delete-dump-file)
(defky (#\C-c #\C-b) byte-compile-file)
