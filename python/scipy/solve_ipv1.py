from scipy.integrate import solve_ivp
import math
import matplotlib.pyplot as plt
import numpy

def plot(f):
    y0 = [0]
    t_span=(-10,10)
    t_eval=numpy.linspace(t_span[0], t_span[1], num=100)
    sol = solve_ivp(f, t_span, y0, t_eval=t_eval)

    #print("t")
    #print(sol.t)
    #print("y1(t)")
    #print(sol.y[0])

    fig, ax = plt.subplots()
    ax.set(xlabel='t', ylabel='y(t)', title=f.__name__)
    ax.grid()

    # plot results
    for i in range(0, len(y0)):
        ax.plot(sol.t, sol.y[i], linewidth=2, label='y{}'.format(i), color='C{}'.format(i))

    plt.legend(loc='best')

#
# Functions to be analyzed
#

def constant(x,y):
    print("x={}, y={}".format(x,y))
    return 1

def linear(x,y):
    return x

def quadratic(x,y):
    return x**2

def exponential(x,y):
    return 2**x

def exponential_decay(t,y):
    return -0.5 * y

def cos(t,y):
    return math.cos(t)

def sin(t,y):
    return math.sin(t)

from scipy.misc import derivative

plot(constant)
plot(linear)
#plot(quadratic)
#plot(exponential)
#plot(exponential_decay)
#plot(cos)
#plot(sin)

plt.show()
