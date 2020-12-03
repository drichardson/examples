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

# Return an array of integrals evaluated over the ranges of [ar[i], ar[i+1]]. The
# result is an array with len(ar)-1 elements.
def integrate_array(f, dx, ar):
    a = 0
    b = 1
    sums = np.zeros(len(ar)-1)
    while b < len(ar):
        sums[a] = integrate(f, dx, ar[a], ar[b])
        a = b
        b += 1
    return sums

# Integrate an array of x positions ar. ar[0] must be 0 and progressive
# entries in the array must move farther from 0. That is, abs(ar[i]) < abs(ar[i+1]).
def integrate_array_from_0(f, dx, ar):
    # integrate_array computes the area under the curve of [ar[i],ar[i+1]],
    # which results in len(sums) = len(ar) - 1...
    sums = integrate_array(f, dx, ar)

    # ...accumulated_sums on the other hand tells you the area under the curve from
    # [0,ar[i]].
    accumulated_sums = np.zeros(len(sums)+1)
    accumulated_sums[0] = 0
    for i in range(0,len(sums)):
        accumulated_sums[i+1] = sums[i] + accumulated_sums[i]
    return accumulated_sums


#def nth_integrate_array_from_0(f, dx, ar, n):
    #sums = integrate_array(f, dx, ar)

# Plot derivatives and integrals of f to order.
def plot(f, order):

    next_color=0
    def nextcolor():
        nonlocal next_color
        color = next_color
        next_color += 1
        return "C{}".format(color)

    DX=0.01
    x = np.linspace(0, np.pi * 2 * 4, 200)
    #x = np.arange(0, 5, DX)

    fig, ax = plt.subplots()
    ax.set(xlabel='x', ylabel='y(x)')
    ax.grid()

    def plot(f, label):
        ax.plot(x, list(map(f, x)), color=nextcolor(), label=label)

    def plotCalculated(y, label):
        ax.plot(x, y, color=nextcolor(), label=label)

    plot(f, 'f')

    # Derivatives
    for n in range(1,order+1):
        plot(nth_derivative(f, DX, n), 'f' + "'" * n)

    # Integrals
    for n in range(1, order+1):
        plotCalculated(integrate_array_from_0(f, DX, x), '∫' * n)
        #plot(nth_integral_from_0(f, DX, n), '∫')

    plt.title('f = {}'.format(f.__name__))
    plt.legend()

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
plot(math.sin, 3)
plot(math.cos, 3)

plt.show()

