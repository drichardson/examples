; SBCL threading example

(defun thread-test ()
  (format t "Starting threads~&")
  (let ((t1 (sb-thread:make-thread
	     (lambda() (loop for i from 1 to 5
			  do (progn (format t "Thread1 call ~a~&" i)
				    (sleep 1))))))
	(t2 (sb-thread:make-thread
	     (lambda() (loop for i from 1 to 3
			  do (progn (format t "Thread2 call ~a~&" i)
				    (sleep 2)))))))
    (sb-thread:join-thread t1)
    (sb-thread:join-thread t2)
    (format t "Threads joined~&")))
