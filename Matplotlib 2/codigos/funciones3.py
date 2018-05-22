import matplotlib.pyplot as plt
import numpy as np

def f(t):
    return np.cos(2*np.pi*t)*(t**2)

t1 = np.arange(0.0, 5.0, 0.05)
print(len(t1))

#GRID 1
plt.subplot(211)
plt.plot(t1, f(t1), 'go-')

#GRID 2
plt.subplot(212)
plt.plot(t1, np.cos(2*np.pi*t1), 'r-')

plt.show()

