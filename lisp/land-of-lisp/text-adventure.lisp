(defparameter *nodes*
  '(
    (living-room (you are in the living room. a wizard
                      is snoring loudly on the couch.))
    (garden (you are in a beautiful garden. there is a
                 well in front of you.))
    (attic (you are in the attic. there is a giant welding
                torch in the corner.))
    ))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defparameter *edges*
  '(
    (living-room (garden west door)
                 (attic upstairs ladder))
    (garden (living-room east door))
    (attic (living-room downstairs ladder))
    ))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location edges)
      (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj) (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
                         `(you see a,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

(defparameter *location* 'living-room)

(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
      (progn (setf *location* (car next))
             (look))
      '(you cannot go that way.))))

(defun nl () (write-char #\linefeed))

(princ "hi")
(princ (look))

;(princ (describe-objects 'living-room *objects* *object-locations*))
;(nl)
;(princ (eq (cadr (assoc 'bucket *object-locations*)) 'living-room))
;(nl)
;(princ (describe-paths 'living-room *edges*))
;(nl)
;(princ (objects-at 'garden *objects* *object-locations*))
;(nl)
;(princ (cdr (assoc 'garden *edges*)))
;(nl)
;(princ (mapcar #'describe-path (cdr (assoc 'living-room *edges*))))
;(nl)
;(princ (describe-location 'living-room *nodes*))
;(nl)
;(princ (describe-path '(garden west door)))
;(nl)
;(princ (describe-paths 'attic *edges*))
;(nl)
