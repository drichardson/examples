package fibonacci

// F
// Compute the nth Fibonacci number
func F(n int) (int) {
	if n == 0 {
		return 0
	} else if n == 1 {
		return 1
	}
	
	return F(n - 1) + F(n - 2)
}

// Series
// Returns a Fibonacci series generator. When the resulting function is called, the next Fibonacci number is returned.
func Series(op func(int64,int64) int64) (func() int64) {
	
	nMinusOne := int64(-1)
	nMinusTwo := int64(-1)
	
	return func() (int64) {	
		
		if nMinusOne == -1 {
			nMinusOne = 0
			return 0
		} else if nMinusTwo == -1 {
			nMinusOne = 1
			nMinusTwo = 0
			return 1
		}
		
		result := op(nMinusOne, nMinusTwo)
		nMinusOne, nMinusTwo = result, nMinusOne
		return result
	}
}
