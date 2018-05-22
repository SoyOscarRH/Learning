import numpy as np
import matplotlib.pyplot as plt

barras = 4
cant_hombres = (13.40, 16.05, 17.67, 18.02)
cant_mujeres = (16.80, 17.80, 18.50, 22.56)


fig, ax = plt.subplots()

#[0 1 2 3 4]
index = np.arange(barras)

bar_width = 0.35

#de 0 a 1
estilo_error = {'ecolor': '0.5'}

plt.bar(index, cant_hombres, bar_width,
                 alpha=1,
                 color='g',
                 yerr=2.5,	
                 error_kw=estilo_error,
                 label='Dolar')

plt.bar(index + bar_width, cant_mujeres, bar_width,
                 alpha=1,
                 color='b',
                 yerr=2.5,
                 error_kw=estilo_error,
                 label='Euro')

#Descripcion de elementos de la grafica
plt.xlabel('Monedas')
plt.ylabel('Precios')
plt.title('Dolar vs Euro')

#Etiquetas del eje x
plt.xticks(index, ('2014', '2015', '2016', '2017'))
plt.legend()


plt.show()