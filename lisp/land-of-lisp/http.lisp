; http server. Based on land of lisp http server. Uses
; SBCL instead of CLISP

(load "url.lisp")

(defun http-parse-request-line (s)
  (let* ((url (subseq s
		      (+ 2 (position #\space s))
		      (position #\space s :from-end t)))
	 (x (position #\? url)))
    (if x
	(cons (subseq url 0 x) (url-parse-params (subseq url (1+ x))))
	(cons url '()))))

(defun http-get-header (stream)
  (multiple-value-bind (line missing-newline-p) (read-line stream)
	(let ((h (let ((i (position #\: line)))
		   (when i
		     (cons (intern (string-upcase (subseq line 0 i)))
			   (subseq line (+ i 2)))))))
	  (if h
	      (cons h
		    (if missing-newline-p
			nil
			(http-get-header2 stream)))
	      nil))))


