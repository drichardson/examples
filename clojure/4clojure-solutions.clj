(require 'clojure.set)

; Re-implement Iterate
(println
	(take 100
		(
			(fn this[f x])
			inc 0)
		)
	) 

(System/exit 1)

; Set Intersection
(println
	(
		; #(clojure.set/difference %1 (clojure.set/union (clojure.set/difference %1 %2) (clojure.set/difference %2 %1)))
		; After reading other solutions, this is the best. You can call a function on a set like
		; this (#{0 1 2 3} 1) and it will return 1 (the value) if found; nil otherwise.
		#(set (filter %1 %2))
		#{0 1 2 3} #{2 3 4 5}
		)
	)

; Map Construction
(println
	(
		(fn [a b] (apply hash-map (flatten (mapv #(vector %1 %2) a b))))
		[:a :b :c] [1 2 3]
		)
	)

; Longest increasing sub-sequence
(println
	(
		(fn[s] (let [l (concat s [0]) r (cons 0 s)] (partition-by (partial = 1) (map - l r))))
		[1 0 1 2 3 0 4 5] ; -> [0 1 2 3]

		; [2 8 3 4 5 0]
		; -
		; [0 2 8 3 4 5]
		; =
		; [2 1 -5 1 1]
		)
	)

; Rotate Sequence
(println
	(
		#(let [[x y]
			(split-at
				(if (> %1 0)
					(rem %1 (count %2))
					(- (count %2) (rem (- %1) (count %2)))
					)
				%2)]
			(concat y x)
			)
		;2 [1 2 3 4 5]
		;6 [1 2 3 4 5]
		;-2 [1 2 3 4 5]
		-4 '(:a :b :c)
		)
	)

; Flipping Out
(println
	(
		(
			#(fn [ & args ] (apply %1 (reverse args)))
			nth
			)
		2 [1 2 3 4 5]
		)
	)

; Split by type
(println
	(
		#(map second (group-by type %))
		[1 :a 2 :b 3 :c]
		)
	)

; Split a sequence
(println
	(
		; Initial solution: #(vector (take %1 %2) (nthnext %2 %1))
		; After learning about drop: #(vector (take %1 %2) (drop %1 %2))
		; After learning about juxt and drop:
		(juxt take drop)
		1 [:a :b :c :d]
		)
	)

; Replicate a sequence
(println
	(
		#(reduce concat (map (fn [x] (repeat %2 x)) %1))
		; Or, using mapcat, which I learned about after seeing the solutions.
		; #(mapcat (fn [x] (repeat %2 x)) %1)
		;[1 2 3] 3
		[[1 2] [3 4]] 3
		)
	)

; Compress a Sequence: Solution 1 before I knew how to use partition-by
(println
	(
		(fn f
			([s] (cons (first s) (f (first s) (rest s))))
			([cur s]
				(if (empty? s)
					'()
					(if (= cur (first s))
						(f cur (rest s))
						(cons (first s) (f (first s) (rest s)))
						)
					)
				)
			)
		;[1 1 2 3 3 2 2 3]
		"Leeeeeroyyy"
		)
	)

; Compress a Sequence: Solution 2 after I learned how to use partition-by
(println
	(
		#(map first (partition-by identity %))
		"Leeeeeroyyy"
		)
	)

; Flatten a Sequence
(println
	(
		#(filter (complement sequential?) (tree-seq sequential? seq %))
		;[[1 2 3] [:a [:b] :c]]
		'((1 2) 3 [4 [5 6]])
		)
	)

; Max element
(println
	(
		(fn this
			([x] x)
			([x y] (if (> x y) x y))
			([x y & more] (let [x1 (this x y) x2 (apply this more)] (if (> x1 x2) x1 x2)))
			)
		1 2 3 4 5 100 -1 5
		)
	)


; Interleave
(println

	(
		(fn this
			([ x1 x2 ] (let [x (first x1) y (first x2)]
				(if (or (= x nil) (= y nil))
					()
					(conj 
						(conj (this (rest x1) (rest x2)) y)
						x)
				)
				)
			)
		) [1 2 3] [:a :b :c])
	)

; Interpose
(println
	(
		(fn this
			([value lst]
				(if (not= (rest lst) [])
					(conj (conj (this value (rest lst)) value) (first lst))
					lst
				)
			)
		)
		:d [1 2]
	)
)

; Drop every nth
(println
	(
		(fn this
			([current seq nth]
				(if (= [] seq)
					(list)
					(if (= current nth)
						; Skip it
						(this 1 (rest seq) nth)
						; Don't skip it
						(conj (this (+ 1 current) (rest seq) nth) (first seq))
						)
					)
				)
			([seq nth] (this 1 seq nth))
			)
		[1 2 3 4 5 6 7 8] 3
		)
	)

; nth
(println
	(
		(fn this
			([counter seq n] (if (= counter n) (first seq) (this (+ counter 1) (rest seq) n)))
			([seq n] (this 0 seq n))
			)
		'(4 5 6 7)
		2
		)
	)

; count
(println
	(
		(fn this
			([counter seq] (if (first seq) (this (+ counter 1) (rest seq)) counter))
			([seq] (this 0 seq))
			)
		'(4 5 6 7 5 55)
		)
	)

; reverse a sequence
(println
	(
		(fn this
			([seq]
				(if (= seq [])
					[]
					(conj (this (rest seq)) (first seq))
					)
				)
			)
		[:a 2 :c :d]
		)
	)

; Sum it all up
(println
	(
		(fn ([args] (apply + args)))
		[1 2 3]
		)
	)

; Generate the nth Fibonacci number
(println
	(
		(fn fib
			([n]
				(if (= n 0)
					0
					(if (= n 1)
						1
						(+ (fib (- n 1)) (fib (- n 2)))
						)
					)
				)
			)
		6)
	)

; Generate a Fibonacci sequence n numbers long
(println
	(
		(fn fibseq
			([count FNminus1 FNminus2]
				(if (= count 0)
					'()
					(conj (fibseq (- count 1) (+ FNminus1 FNminus2) FNminus1) (+ FNminus1 FNminus2))
					)
				)
			([count]
				(if (= count 0)
					[]
					(if (= count 1)
						'(1)
						(if (= count 2)
							'(1 1)
							(concat '(1 1) (fibseq (- count 2) 1 1))
							)
						)
					)
				)
			)
		10
		)
	)

; Generate a Fibonacci sequence n numbers long (2nd try using iterate)
(println
	(
		#(take %1 (map first (iterate (fn [[f s]] [s (+ f s)]) [1 1])))
		12
		)
	)

; Palindrome Detector
(println
	(#(= (seq %1) (reverse %1)) [ 1 2 3 2 1])
	)

