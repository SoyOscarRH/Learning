import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

fig, ax = plt.subplots()
x = np.arange(0, 2*np.pi, 0.01)
line, = ax.plot(x, np.sin(x), 'r')


def actualiza(i):
    line.set_ydata(np.sin(x + i/10.0))
    return line,

def init():
    line.set_ydata(np.ma.array(x))
    return line,

ani = animation.FuncAnimation(fig, actualiza, init_func=init, interval=2)

plt.show()
