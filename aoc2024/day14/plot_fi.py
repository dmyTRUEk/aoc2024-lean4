import matplotlib.pyplot as plt

from plot_fi_data import *

# print(data)
data = data_2x2
# data = data_3x3

plt.plot(range(len(data)), data)
plt.show()

