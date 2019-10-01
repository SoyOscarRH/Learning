import numpy as np


h1s = np.array([0.8481288363433407, 0.032926394798136256,
                0.032295464698450495, 0.00020342697805520653])
h2s = np.array([0.005220125693558397, 0.13238887354206538,
                0.13354172253321245, 0.8175744761936437])

for i, hi in enumerate([h1s, h2s]):
  mean = np.mean(hi)
  std = np.std(hi)
  std_like = round(std, 3)

  print(f"i={i+1} \t mean={mean} \t std={std_like}")
  print(np.around((hi - mean) / std, 3))
  print(np.around((hi - mean) / std_like, 3), end="\n\n")

  

