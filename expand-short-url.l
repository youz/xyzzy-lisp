(require 'cmu_loop)

(in-package "user")
(defun expand-short-url (url)
  (if (not (string-match "http://\\([^/]+\\)/\\(.+\\)" url))
      url
    (let ((host (match-string 1))
	  (path (match-string 2)))
      (with-open-stream (cn (connect host 80))
	(format cn "GET /~A HTTP/1.1\nHost: ~A\n\n" path host)
	(let* ((res (read-line cn nil))
	       (m (string-match "^HTTP/[0-9.]+ \\([0-9]+\\) .+$" res))
	       (status (match-string 1)))
	  (if (not (and m (find status '("301" "302") :test #'string=)))
	      (values url res)
	    (loop
	      for buf = (read-line cn nil)
	      when (string-match "^Location: \\(.+\\)$" buf)
	      do (return (match-string 1))
	      while buf)))))))
(export '(expand-short-url))

(provide "expand-short-url")
