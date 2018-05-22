import numpy as np
import matplotlib.pyplot as plt

a = 1.
theta = np.linspace(0, 2.*np.pi, 1000)
r = 2 * a * (1. + np.cos(theta))
ax = plt.subplot(111, projection='polar')
ax.plot(theta, r)
ax.grid(True)

plt.show()