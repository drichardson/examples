# If TEST123 is only set in the makefile, it will not be in the environment.
# However, if TEST123 is set in the environment before running make, then it will
# be overriden by the TEST123=lala assignment above. For details, see:
# https://www.gnu.org/software/make/manual/html_node/Environment.html
#
TEST123=lala

environment:
	python3 -c 'import os; print(os.getenv("TEST123"))'
