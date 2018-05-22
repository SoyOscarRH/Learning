import matplotlib.pyplot as plt
import numpy as np

x = np.linspace(0, 2 * np.pi, 200)
y = np.sin(x)
z = x**2
final = np.sin(z)

f, axarr = plt.subplots(2, 2)

#Primer cuadrante
axarr[0, 0].plot(x, y)
axarr[0, 0].set_title('cuadrante [0,0]')

#Segundo cuadrante
axarr[0, 1].scatter(x, z)
axarr[0, 1].set_title('cuadrante [0,1]')

#Tercer cuadrante
axarr[1, 0].plot(x, final)
axarr[1, 0].set_title('cuadrante [1,0]')

#Cuarto cuadrante
axarr[1, 1].scatter(x, final)
axarr[1, 1].set_title('cuadrante [1,1]')

'''
for ax in axarr.flat:
    ax.set(xlabel='eje x', ylabel='eje y')
'''

print(len(axarr))
i=0
j=0
fig=1
while(i<len(axarr)):
	j=0
	while(j<len(axarr)):
		axarr[i, j].set_xlabel('eje x figura'+ str(fig))
		axarr[i, j].set_ylabel('eje y')
		print(str(i) + "," + str(j))
		j+=1
		fig+=1
	i+=1

'''
axarr[0, 0].set_xlabel('eje x figura 1')
axarr[0, 0].set_ylabel('eje y figura 1')
axarr[0, 1].set_xlabel('eje x figura 2')
axarr[0, 1].set_ylabel('eje y figura 2')
axarr[1, 0].set_xlabel('eje x figura 3')
axarr[1, 0].set_ylabel('eje y figura 3')
axarr[1, 1].set_xlabel('eje x figura 4')
axarr[1, 1].set_ylabel('eje y figura 4')
'''


for ax in axarr.flat:
    ax.label_outer()


#Otra forma
f,((g1, g2), (g3, g4), (g5, g6) )=plt.subplots(3, 2, sharex='col', sharey='row')
f.suptitle('Graficas de m (renglones) x n (columnas)')
g1.plot(x, y, color='red')
g2.scatter(x, y, color='red')
g3.plot(x, z, color='cyan')
g4.scatter(x, z, color='cyan')
g5.plot(x, final, color='green')
g6.scatter(x, final, color='green')


plt.show()