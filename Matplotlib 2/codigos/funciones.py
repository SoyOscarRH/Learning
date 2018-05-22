import matplotlib.pyplot as plt
import numpy as np

#[a, b]
#cantidad de valores que quiero en mi intervalo
x = np.linspace(0, 2, 100)
print(x)
print len(x)


#Graficas
plt.plot(x, x, 'r', label='x=y')
plt.plot(x, x**2, 'g',label='x^2')
plt.plot(x, (3*x)+10, 'y' ,label='3x+10')
plt.plot(x, ((2*x)/4)-1, color='purple' ,label='(2x/4)-1')

#Etiques de los ejes 'x' y 'y'
plt.xlabel('eje x')
plt.ylabel('eje y')

#Titulo de la grafica
plt.title("Graficas de funciones basicas")

#Colocar recuadro de informacion
plt.legend()

#Mostrar
plt.show()