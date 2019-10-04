import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import warnings

from IPython.core.pylabtools import figsize

def muestraImagen(vector3D, labelsVector, indice):
    figsize(3, 3)
    plt.title(labelsVector[indice])
    # El -1 invierte el orden en y
    plt.pcolormesh(vector3D[indice][::-1], cmap=cm.winter)

def muestraActividad(red, iEntrada):
    """ Grafica los valores de activación de cada neurona
    para la entrada en la columna iEntrada.
    """
    if(iEntrada > red.A0.shape[1]):
        raise IndexError("Ejemplar de entrenamiento inexistente " + str(iEntrada))
    nRens = 4
    nCols = 1

    fig, axes = plt.subplots(figsize=(6,4))
    norm = matplotlib.colors.Normalize(vmin=0, vmax=1)

    ax_0 = plt.subplot2grid((nRens,nCols), (2,0), rowspan=2)
    ax_1 = plt.subplot2grid((nRens,nCols), (1,0))
    ax_2 = plt.subplot2grid((nRens,nCols), (0,0), sharey=ax_1)

    a0 = red.A0[:,iEntrada]
    a1 = red.A1[:,iEntrada:iEntrada+1].T
    a2 = red.A2[:,iEntrada:iEntrada+1].T

    # A0
    ax_0.pcolormesh(a0[1:].reshape((28,28))[::-1], cmap=cm.cool, norm=norm)
    ax_0.set_xlim(0, 28)
    ax_0.set_ylim(0, 28)

    # A1
    ax_1.pcolormesh(a1, cmap=cm.cool, norm=norm)
    ax_1.set_yticks(np.array([0,1]))
    ax_1.set_xlim(0, 26)
    ax_1.set_xticks(np.arange(26) + 0.5)
    ax_1.set_xticklabels(np.arange(26), minor=False, ha='center')

    # A2
    ax_2.pcolormesh(a2, cmap=cm.cool, norm=norm)
    ax_2.set_xticks(np.arange(10) + 0.5)
    ax_2.set_xticklabels(np.arange(10), minor=False, ha='center')

    # Barra de color
    ax1 = fig.add_axes([1.0, 0, 0.025, 1.0]) # left, bottom, width, height
    cb1 = matplotlib.colorbar.ColorbarBase(ax1, cmap=cm.cool,
                                norm=norm,
                                orientation='vertical')

    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        plt.tight_layout()