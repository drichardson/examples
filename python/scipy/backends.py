
# https://matplotlib.org/tutorials/introductory/usage.html#sphx-glr-tutorials-introductory-usage-py

import matplotlib
import matplotlib.pyplot as plt
import numpy as np

def my_plotter(ax, data1, data2, param_dict={}):
    ax.plot(data1, data2, **param_dict)
    ax.set(xlabel='Time (s)', ylabel='Amplitutde', title='Sine')
    ax.grid()

def test_backend(backend):
    matplotlib.use(backend)
    t = np.arange(0.0, 2.0, 0.01)
    s = 1 + np.sin(2 * np.pi * t)
    fig, ax = plt.subplots()
    my_plotter(ax, t, s)
    fig.savefig("sine-backend.{}".format(backend))


test_backend('pdf')
test_backend('svg')
test_backend('ps')

