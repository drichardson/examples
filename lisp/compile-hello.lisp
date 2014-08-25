; invoke from compile-hello.sh

(defun mymain ()
  (fresh-line)
  (princ "Hi from my main"))

(save-lisp-and-die "myhello" :executable t :toplevel #'mymain)


