(defstruct person name age)
(defstruct (newborn (:include person (age 1))))
(defstruct (centenarian (:include person (age 100))))
(defstruct (kid (:include person (age 10))))

(defmethod person-talk ((p person)) `(,(type-of p) says hi))
(defmethod person-talk ((p newborn)) 'WAAAAAAA!)
(defmethod person-talk ((p centenarian))
  "Hi there sonny")

(defun test-people ()
  (mapcar #'person-talk (list
			 (make-person)
			 (make-newborn)
			 (make-centenarian)
			 (make-kid)))
  )

(test-people)
