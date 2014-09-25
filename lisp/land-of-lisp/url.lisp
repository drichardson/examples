; url utilities. Based on land of lisp http server code but
; modified to be more compliant with RFC 3986

; convert hex 2 characters into it's character representation
(defun url-unescape-2digits (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer (coerce (list c1 c2) 'string)
			     :radix 16
			     :junk-allowed t)))
    (if code
	(code-char code)
	default)))

; convert a string that contains UTL
(defun url-decode-param (s)
  (labels ((f (1st)
	     (when 1st
	       (case (car 1st)
		 (#\% (cons (url-unescape-2digits (cadr 1st) (caddr 1st))
			    (f (cdddr 1st))))
		 (#\+ (cons #\space (f (cdr 1st))))
		 (otherwise (cons (car 1st) (f (cdr 1st))))))))
    (coerce (f (coerce s 'list)) 'string)))



; parse query parameters of the form "key1=value1&key2=value%202"
(defun url-parse-params (s)
  (let* ((i1 (position #\= s))
	 (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
			  (url-decode-param (subseq s (1+ i1) i2)))
		    (and i2 (url-parse-params (subseq s (1+ i2))))))
	  ((equal s "") nil)
	  (t s))))
