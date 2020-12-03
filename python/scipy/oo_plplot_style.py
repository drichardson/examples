# https://matplotlib.org/tutorials/introductory/usage.html#the-object-oriented-interface-and-the-pyplot-interface

import matplotlib
import matplotlib.pyplot as plt
import numpy as np

x = np.linspace(0,2,100)

#
# OO style plplot interface
#

fig, ax = plt.subplots() # Create a figure and an axes.
ax.plot(x, x, label='linear')
ax.plot(x, x**2, label='quadratic')
ax.plot(x, x**3, label='cubic')
ax.set_xlabel('x label')
ax.set_ylabel('y label')
ax.set_title('Simple Plot - oo-style')
ax.legend()

print("Showing OO style plplot interface")
plt.show()

plt.plot(x, x, label='linear')
plt.plot(x, x**2, label='quadratic')
plt.plot(x, x**3, label='cubic')
plt.xlabel('x label')
plt.ylabel('y label')
plt.title('Simple Plot - plplot-style')
print("Showing plplot style interface")
plt.show()

print("All done")
