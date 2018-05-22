import matplotlib.pyplot as plt
import numpy as np

x = np.linspace(0, 2 * np.pi, 400)
y = np.sin(x ** 2)
v1 = np.arange(1, 10, 1)

f, axarr = plt.subplots(3, sharex=True)
f.suptitle('Graficas horizontales')
axarr[0].plot(x, y, color='red')
axarr[1].scatter(x, y)
axarr[2].plot(v1, v1, color=('#FF3089'), linewidth=3 )

#Ejercicio 2
#Agrega una funcion x=y, en un intervalo de 0 10 #que vaya despues de la funcion almacenada en axarr1

f, (ax1, ax2, ax3) = plt.subplots(1, 3, sharey=True)
f.suptitle('Graficas verticales')
ax1.plot(x, y, color='red')
ax2.scatter(x, y)
ax3.plot(v1, v1, color='green', marker='H',linestyle='dotted' , linewidth=3 )


plt.show()