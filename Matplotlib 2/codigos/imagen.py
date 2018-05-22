import matplotlib.pyplot as plt
import matplotlib.image as mpimg
import numpy as np

img = mpimg.imread('/home/cinthia/Escritorio/manzana.jpeg')

#print(img)


#Normal
#imgplot = plt.imshow(img)


#Muestra 1
lum_img = img[:, :, 0]
#plt.imshow(lum_img)


#Muestra 2
plt.imshow(lum_img, cmap="hot")


plt.show()