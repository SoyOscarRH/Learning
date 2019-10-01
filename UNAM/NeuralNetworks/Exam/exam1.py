import numpy as np

x1 = np.array([-1, -2, 1, 1.5])
x2 = np.array([203, 405, 126, 228])
x3 = np.array([0.01, 0.22, 1.23, -0.2])
x4 = np.array([-400, 328, -52, -25])

for i, xi in enumerate([x1, x2, x3, x4]):
  mean = np.mean(xi)
  std = np.std(xi)
  std_like = round(std, 3)

  print(f"i={i+1} data={xi} \t\t mean={mean} \t std={std_like}")
  print((xi - mean) / std)
  print((xi - mean) / std_like, end="\n\n")