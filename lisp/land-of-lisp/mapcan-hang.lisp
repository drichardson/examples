
; Just testing out mapcan. It seems to hang a lot
; See this: http://stackoverflow.com/questions/20207041/why-does-this-mapcan-cause-my-repl-to-freeze

; The problem is that the quoted lists inside the lambda are evaluated
; once. Then, after being modified by nconc (used by mapcan/mapcon) they
; create circular lists.

(defun ingredients-original (order)
  (mapcan (lambda (burger)
            (case burger
              (single '(patty))
              (double '(patty patty))
              (double-cheese '(patty patty cheese))))
          order))

; If you use the list function instead of quoted list, a new list
; will be created every time the lambda is called, rather than when
; the lambda is created, thus the problem works fine.
(defun ingredients-using-list (order)
  (mapcan (lambda (burger)
            (case burger
              (single (list 'patty))
              (double (list 'patty 'patty))
              (double-cheese (list 'patty 'patty 'cheese))))
          order))


; Here's an instrumented version of the original showing that
; (in CLISP at least) the quoted list is reused.
(defun ingredients-debug (order)
  (let ((seen nil))
    (mapcan (lambda (burger)
              (let ( (result (case burger
                               (single '(patty))
                               (double '(patty patty))
                               (double-cheese '(patty patty cheese)))))
                (fresh-line)
                (princ "result is ")
                (princ result)
                (if (member result seen)
                  (princ " already seen")
                  (progn (princ " adding") (push result seen)))
                result
                )
              )
            order)))
