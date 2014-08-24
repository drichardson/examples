; example using sockets in SBCL
; to test, run (main) and then from a shell use netcat:
; $ nc localhost 8080

(require 'sb-bsd-sockets)
(defparameter *localhost-address* '(127 0 0 1))

(defun make-listen-socket ()
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket *localhost-address* 8080)
    (sb-bsd-sockets:socket-listen socket 1)
    socket))

(defparameter *data* (format nil "test~%"))

(defun accept-one (l)
  (let ((c (sb-bsd-sockets:socket-accept l)))
    (unwind-protect
	 (sb-bsd-sockets:socket-send c *data* (length *data*) :external-format :utf-8)
      (sb-bsd-sockets:socket-close c))))

(defun accept-one-stream (l)
  (let ((c (sb-bsd-sockets:socket-accept l)))
    (unwind-protect
	 (let ((stream (sb-bsd-sockets:socket-make-stream c :output t)))
	   (format stream "test~&"))
      (sb-bsd-sockets:socket-close c))))

(defun runloop (l)
  ;(accept-one l)
  (accept-one-stream l)
  (runloop l))

(defun main ()
  (let ((l (make-listen-socket)))
    (unwind-protect
	 (runloop l)
      (progn (format t "~&Closing listen socket~%")
	     (sb-bsd-sockets:socket-close l)))))
