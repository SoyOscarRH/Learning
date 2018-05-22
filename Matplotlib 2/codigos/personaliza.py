import matplotlib.pyplot as plt
import numpy as np

x = np.linspace(0, 2 * np.pi, 400)
y = np.sin(x ** 2)

plt.subplots(facecolor='lightblue')
plt.suptitle('Graficas horizontales', fontsize=20, color='white')

ax = plt.subplot(111, axisbg='pink')
plt.plot(x,y, 'r-', linewidth=2 )

label_x = plt.xlabel("eje x")
label_y = plt.ylabel("eje y")
label_x.set_color("red")
label_y.set_color("blue")

ax.spines['bottom'].set_color('#FF0000')
ax.spines['top'].set_color('green')
ax.spines['left'].set_color('yellow')
ax.spines['right'].set_color('yellow')

ax.tick_params(axis='x', colors='red')
ax.tick_params(axis='y', colors='purple')

plt.show()