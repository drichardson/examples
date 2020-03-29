from scipy.integrate import solve_ivp
import matplotlib.pyplot as plt
import numpy

def exponential_decay(t,y):
    return -0.5 * y

y0 = [2,4,8]
t_span=(0,10)
t_eval=numpy.linspace(t_span[0], t_span[1], num=50)
sol = solve_ivp(exponential_decay, t_span, y0, t_eval=t_eval)

print("t")
print(sol.t)
print("y1(t)")
print(sol.y[0])

# plot results
for i in range(0, len(y0)):
    plt.plot(sol.t, sol.y[i], linewidth=2, label='y{}'.format(i), color='C{}'.format(i))

plt.xlabel('time')
plt.ylabel('y(t)')
plt.legend(loc='best')
plt.show()
