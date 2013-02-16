def make_incrementor(n):
	return lambda x: x + n

f = make_incrementor(3)
g = make_incrementor(10)

print "f(5) = %i" % f(5)
print "g(5) = %i" % g(5)

primes = [1, 3, 5, 7, 11, 13, 17, 19]

print "The primes are " + ", ".join(map(str, primes))
print "The primes to the power of 2 are " + ", ".join(map(str, map(lambda x: x**2, primes)))
