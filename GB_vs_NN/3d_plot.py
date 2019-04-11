import matplotlib.pyplot as plt
import numpy as np
from mpl_toolkits.mplot3d import Axes3D

fig = plt.figure()

x = np.repeat(np.array(range(0, 101), dtype = float), 101).reshape((101, 101))
x = x/100
y = x.transpose()

def calc(x, y):
    z = x*(1 - x) * (1 - y)*(1-y)
    z += x * (1 - x) * (1 - y) *(1-y)
    z += y * (1 - y) * (1 - x) * (1-x)
    z += x * x * y * (1 - y)
    z += y * y * x * (1 - x)
    z += 2 * (x * x * (1 - y) * (1-y) + y *y * (1 - x) *(1-x))
    z *= 0.5
    z -= x * (1 - x)
    return z

ax = Axes3D(fig)
ax.plot_wireframe(x, y, calc(x,y), rstride = 2, cstride = 2)

ax.set_xlabel('x')
ax.set_ylabel('y')
ax.set_zlabel('g(x,y) - f(x)')

plt.show()