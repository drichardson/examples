# print()
# https://docs.python.org/3/library/functions.html#print
print('literal single quotes')
print("literal double quotes")

my_var=123

# printf style formatting (try to use use other formatters below instead)
# https://docs.python.org/3/library/stdtypes.html#printf-style-string-formatting
print('printf-style single value: %s' % 'Value1')
print('printf-style single value (tuple): %s' % ('Value1',))
print('printf-style two values: %s, %d' % ('Value1', my_var))

# str.format()
# https://docs.python.org/3/library/stdtypes.html#str.format
# https://docs.python.org/3/library/string.html#formatstrings
print('str.format no keys: {}'.format('Value1'))
print('str.format single key: {x}'.format(x='Value1'))
print('str.format two keys: {x}, {y}'.format(x='Value1', y=my_var))
print('str.format two keys repeated: {x}, {y}, {y}, {x}'.format(x='Value1', y=my_var))

# Formatted string literals (aka f-strings)
# https://docs.python.org/3/reference/lexical_analysis.html#f-strings
# https://www.python.org/dev/peps/pep-0498/
print(f'f-string one value {"Value1"}')
print(f'f-string multiple values {"Value1"}, {my_var}')

# Template strings
from string import Template
t = Template('template string one substitution: $who likes $what')
print(t.substitute(dict(who='tim', what=my_var)))
print(t.substitute(dict(who='joe', what='hamburgers')))

print('''My
multiline
string
''')

print(f'''My
multiline {my_var}
f-string''')

