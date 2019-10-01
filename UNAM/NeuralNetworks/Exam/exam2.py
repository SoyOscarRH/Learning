import numpy as np

x1 = 1
x2 = 1


def sigm(x): return 1 / (1 + np.exp(-x))


# input
a0 = 1
a1 = x1
a2 = x2

# w input - layer 1
w01 = 1.72
w02 = -5.25

w11 = -5.12
w12 = 3.38

w21 = -5.1
w22 = 3.37

# layer 1
zh1 = w01 * a0 + w11 * a1 + w21 * a2
zh2 = w02 * a0 + w12 * a1 + w22 * a2
print(f"zh1 = {round(zh1, 2)}")
print(f"zh2 = {round(zh2, 2)}")

h0 = 1
h1 = sigm(zh1)
h2 = sigm(zh2)
print(f"h1 = {h1}")
print(f"h2 = {h2}")



# w layer 1 - layer 2
w0o = -3.2
w1o = 6.6
w2o = 6.5

# layer 1
zho = w0o * h0 + w1o * h1 + w2o * h2
o1 = sigm(zho)
print(f"zho = {round(zho, 2)}")
print(f"o1 = {round(o1, 3)}")
