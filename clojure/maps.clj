; A nil key, #134
; Write a function which, given a key and map, returns true iff the map contains an entry with that key and its value is nil.
(println
	(#(let [x (find %2 %1)] (and (not= x nil) (= (second x) nil)))
		:a {:a nil :b 2}
		)
	)

; Infix Calculator
(println
	((fn this
		([l op r & more] (apply this (op l r) more))
		([l] l)
		)
		38 + 48 - 2 / 2)
	)