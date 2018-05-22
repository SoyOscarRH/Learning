import matplotlib.pyplot as plt
import numpy as np

t = np.linspace(0, 2*np.pi, 400)
s1 = np.sin(t)

#Grafica de la funcion seno
plt.plot(t, s1, color='green', label='funcion sen(x)')
plt.legend()
plt.show()