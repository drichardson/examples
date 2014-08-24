; http server. Based on land of lisp http server. Uses
; SBCL instead of CLISP

(load "url.lisp")
(load "sockets.lisp")

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
			(http-get-header stream)))
	      nil))))


(defun http-parse-post-body (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
	(read-sequence content stream)
	(url-parse-params content)))))

(defun http-serve (request-handler)
  (let ((socket (socket-listen 8080)))
    (unwind-protect
	 (loop (with-open-stream (stream (socket-accept socket))
		 (let* ((url (http-parse-request-line (read-line stream)))
			(path (car url))
			(header (http-get-header stream))
			(params (append (cdr url)
					(http-parse-post-body stream header))))
		   (funcall request-handler stream path header params))))
      (socket-server-close socket))))

(defun hello-request-handler (stream path header params)
  (format stream "response~&~A, ~A, ~A" path header params))

(defun main-hello-http ()
  (http-serve #'hello-request-handler))

