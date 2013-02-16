# An r or R prefix to a string disables normal C escapes (i.e. r"\n" is actually backslash followed
# by the letter n, rather than a newline).

import re

digitRegEx = re.compile('[0-9]+')

tryToMatch = ["12345", "Douglas", "12Doug", "Doug12"]

for x in tryToMatch:
	print "\nMatching " + x
	m = digitRegEx.match(x)
	if m:
		print "m.group() = %s, m.start() = %s, m.end() = %s" % (m.group(), m.start(), m.end())
	else:
		print "Match not found"

