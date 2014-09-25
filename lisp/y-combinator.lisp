; from http://rosettacode.org/wiki/Y_combinator#Common_Lisp

(defun Y (f)
  ((lambda (x) (funcall x x))
   (lambda (y)
     (funcall f (lambda (&rest args)
		  (apply (funcall y y) args))))))


(defun fac (f)
  (lambda (n)
    (if (zerop n)
	1
	(* n (funcall f (1- n))))))

(mapcar (Y #'fac) '(1 2 3 4 5 6))
