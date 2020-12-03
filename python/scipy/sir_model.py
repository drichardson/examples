# Following along to [The Coronavirus Curve -
# Numberphile](https://www.youtube.com/watch?v=k6nLfCbAzgo) but using Python
# instead of geogebra.

import matplotlib
import matplotlib.pyplot as plt
import numpy as np
from scipy.integrate import solve_ivp

#
# With the SIR model, we start with assumptions about percentage of
# susceptible, infected, and removed (recovered/dead) and other model
# parameters such as transmission and recovery rates.
#
# We are trying to develop/graph functions for S(t), I(t), and R(t) that will
# predict the number of susecptible, infected, and removed cases at time t.

#
# This method uses PySci's solve_ivp, which solves an initial value problem for
# a system of ODEs.
#
# https://www.jirka.org/diffyqs/html/sec_introtosys.html
# https://en.wikipedia.org/wiki/Ordinary_differential_equation#System_of_ODEs
# https://www.youtube.com/watch?v=i5sxsJjfDBQ
#

# Initial Values
I0=0.01
S0=1-I0
R0=0

# Rate Parameters
transmission_rate=3.2
recovery_rate=0.23

# Simulation Length
max_time=20


#
# Rate equations
#
def dsdt(S, I, R):
    return -transmission_rate * S * I

def didt(S, I, R):
    return transmission_rate * S * I - recovery_rate * I

def drdt(S, I, R):
    return recovery_rate * I

#
# The SIR model, for use by solve_ivp.
#
def SIR(t, y):
    S = y[0]
    I = y[1]
    R = y[2]
    return [dsdt(S,I,R), didt(S,I,R), drdt(S,I,R)]

# Run the solver. max_step isn't necessary, but a lower value is used to
# smooths out the results.
sol=solve_ivp(SIR, [0, max_time], [S0,I0,R0])

#
# Plot the simulation
#
fig, ax = plt.subplots()
ax.plot(sol.t, sol.y[0], label='S', color='C0')
ax.plot(sol.t, sol.y[1], label='I', color='C1')
ax.plot(sol.t, sol.y[2], label='R', color='C2')
ax.set(xlabel='time', ylabel='population', title='SIR Simulation')
ax.grid()
plt.legend()
plt.show()

