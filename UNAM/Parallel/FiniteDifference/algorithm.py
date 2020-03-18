from matplotlib import pylab
import numpy as np
from scipy import linalg

steps_x = 50
steps_t = 100

time_max = 1.0
x_max = 2.0


def initialCondition(x):
    return x * x * (2 - x)

# initialize the U value matrix
temperature_at = np.zeros([steps_x + 1, steps_t + 1])
points_x = np.linspace(0, x_max, steps_x + 1)

# initilise the x_initial values for initial condiitons
nodes_x_ini = np.linspace(0, x_max, steps_x + 1)
temperature_at[:, 0] = list(map(initialCondition, nodes_x_ini))

dx = x_max / steps_x
dt = time_max / steps_t
print(dt)
kappa = 4.0

rho = kappa * dt / (dx * dx)


for k in range(0, steps_t):
    mat_dig = np.zeros([steps_x+1, steps_x+1])

    for i in range(1, steps_x, 1):
        mat_dig[i][i] = 1. + (2. * rho)
        mat_dig[i][i-1] = - rho
        mat_dig[i][i+1] = - rho

    mat_dig[0][0] = 1.
    mat_dig[steps_x][steps_x] = 1.

    rhs = temperature_at[:, k]
    temperature_at[:, k + 1] = linalg.solve(mat_dig, rhs)
    print(mat_dig)
    print(rhs)

    exit(0)


# plot the graph
pylab.figure(figsize=(12, 6))
pylab.xlabel('$x$', fontsize=15)
pylab.ylabel(r'$U(\dot, \tau)$', fontsize=15)

pylab.plot(points_x, temperature_at[:, int(0.00 / dt)], color="red")
pylab.plot(points_x, temperature_at[:, int(0.05 / dt)], color="blue")
pylab.plot(points_x, temperature_at[:, int(0.10 / dt)], color="green")
pylab.legend([r'$\tau = 0.0$', r'$\tau = 0.05$', r'$\tau = 0.10$'], fontsize=15)

pylab.show()
