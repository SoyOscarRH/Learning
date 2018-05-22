import matplotlib.pyplot as plt
import numpy as np

t = np.arange(0., 5., 0.2)

plt.plot(t, t, 'r--', t, t**2, 'bs-', t, t**3, 'g^--', linewidth=5)

'''
plt.plot(t, t, 'r--')
plt.plot(t, t**2, 'bs-')
plt.plot(t, t**3, 'g^')
'''

plt.show()