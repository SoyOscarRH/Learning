import numpy as np
import matplotlib.pyplot as plt

x = np.linspace(0, 2 * np.pi, 400)
y = np.sin(x ** 2)

plt.plot(x,y)

#Agregamos las etiquetas de texto
plt.text(1, -0.5, r'$\sin{\sqrt{\frac{x^2}{y_{4}^2}}}$', fontsize=20)

plt.xlabel('eje x')
plt.ylabel('eje y')

plt.show()