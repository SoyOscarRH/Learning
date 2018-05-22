#REALIZA UN PROGRAMA QUE GRAFIQUE LA FUNCION COSENO DE 0 A 4PI COLOR ANARANJADO ESTILO DE LINEA PUNTEADO

import matplotlib.pyplot as plt
import numpy as np

t = np.linspace(0, 4*np.pi, 400)
s1 = np.cos(t)

#Grafica de la funcion seno
plt.plot(t, s1, color='orange', linestyle='dashed', label='funcion coseno', linewidth=2)


plt.title('EJERCICIO 1')
plt.legend()
plt.show()
