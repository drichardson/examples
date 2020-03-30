import math
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import scipy.integrate


#
# Differentiation
#

# Differentiate f at x.
def differentiate(f, dx, x):
    dy = f(x+dx) - f(x)
    return dy / dx

# Return the derivitive of f.
def derivative(f, dx):
    return lambda x: differentiate(f, dx, x)

# Return the nth derivative of f.
def nth_derivative(f, dx, n):
    for _ in range(n):
        f = derivative(f, dx)
    return f

#
# Integration
#

# Numerically integrate f over the interval [a,b] in increments of dx.
def integrate(f, dx, a, b):
    xN = max(a,b)
    x = min(a,b)
    r = 0
    while x <= xN:
        r += f(x) * dx
        x += dx
    return r

# Return a function that can calculate the definite integral of
# f over the range [a,b].
def integral(f, dx):
    return lambda a, b: integrate(f, dx, a, b)

def integral_from_0(f, dx):
    return lambda b: integrate(f, dx, 0, b)

def nth_integral_from_0(f, dx, n):
    for _ in range(n):
        f = integral_from_0(f, dx)
    return f

# Plot derivatives and integrals of f to order.
def plot(f, order):

    next_color=0
    def nextcolor():
        nonlocal next_color
        color = next_color
        next_color += 1
        return "C{}".format(color)

    DX=0.01
    x = np.linspace(0, np.pi * 2 * 4, 80)
    #x = np.arange(0, 5, DX)

    fig, ax = plt.subplots()
    ax.set(xlabel='x', ylabel='y(x)')
    ax.grid()

    def plot(f, label):
        ax.plot(x, list(map(f, x)), color=nextcolor(), label=label)

    plot(f, 'f')

    # Derivatives
    for n in range(1,order+1):
        plot(nth_derivative(f, DX, n), 'f' + "'" * n)

    # Integrals
    for n in range(1, order+1):
        plot(nth_integral_from_0(f, DX, n), '∫')

    plt.title('f = {}'.format(f.__name__))
    plt.legend()

    #jfig, ax = plt.subplots()
    #my_plotter(ax, x, x)
    #plt.title('again')
    #plt.legend()


#
# Functions to be analyzed
#

def constant(x):
    return 1

def linear(x):
    return x

def quadratic(x):
    return x**2

def exponential(x):
    return 2**x

plot(constant, 1)
plot(linear, 1)
plot(quadratic, 2)
plot(exponential, 2)
plot(math.sin, 2)
plot(math.cos, 2)

plt.show()

