import numpy as np
import matplotlib.pyplot as plt
import mpl_toolkits.mplot3d.axes3d as axes3d

fig = plt.figure()
ax = fig.add_subplot(1, 1, 1, projection='3d')

u = np.linspace(0, 2*np.pi, 60)
v = np.linspace(0, 2*np.pi, 60)
U, V = np.meshgrid(u, v)

X = U
Y = (U**2)*np.cos(V)
Z = (U**2)*np.sin(V)

ax.plot_surface(X, Y, Z, alpha=0.3, color='red')

plt.show()