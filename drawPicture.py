from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import matplotlib

matplotlib.use('TkAgg')


def cuboid_data2(o, size=(1, 1, 1)):
    X = [[[0, 1, 0], [0, 0, 0], [1, 0, 0], [1, 1, 0]],
         [[0, 0, 0], [0, 0, 1], [1, 0, 1], [1, 0, 0]],
         [[1, 0, 1], [1, 0, 0], [1, 1, 0], [1, 1, 1]],
         [[0, 0, 1], [0, 0, 0], [0, 1, 0], [0, 1, 1]],
         [[0, 1, 0], [0, 1, 1], [1, 1, 1], [1, 1, 0]],
         [[0, 1, 1], [0, 0, 1], [1, 0, 1], [1, 1, 1]]]
    X = np.array(X).astype(float)
    for i in range(3):
        X[:, :, i] *= size[i]
    X += np.array(o)
    return X


def plotCubeAt2(positions, sizes=None, colors=None, **kwargs):
    if not isinstance(colors, (list, np.ndarray)): colors = ["C0"] * len(positions)

    if not isinstance(sizes, (list, np.ndarray)): sizes = [(1, 1, 1)] * len(positions)
    g = []
    for p, s, c in zip(positions, sizes, colors):
        g.append(cuboid_data2(p, size=s))
    return Poly3DCollection(np.concatenate(g), facecolors=np.repeat(colors, 6), **kwargs)


# positions = [(-3, 6, -2), (2, 7, 1), (1, 1, 1)]
# sizes = [(4, 5, 3), (3, 3, 7), (2, 7, 1)]
# colors = ["crimson", "limegreen", "limegreen"]
positions = []
sizes = []
instances = pd.read_csv("C:\Lindoapi\python\pyLindo\MasterTHESIS\SampleInstances.csv")
points = [[8, (0, 0, 0)], [7, (4.823407950584469, 0, 0)], [1, (0, 23.4425176663484, 0)], [12, (8.123940849152289, 0, 23.4425176663484)], [11, (0, 23.4425176663484, 10.1828571428577)], [6, (4.823407950584469, 23.4425176663484, 10.1828571428577)], [0, (9.646815901168939, 23.4425176663484, 0)], [9, (17.99961958921785, 21.8277084016885, 0)], [5, (0, 21.8277084016885, 36.190477784352396)], [10, (44.31441863350469, 0, 0)], [2, (35.928191017789246, 23.4425176663484, 0)], [13, (17.99961958921785, 32.6522188024146, 41.01388573493686)]]

# [[1, (0, 0, 0)], [2, (0, 8.123940849152289, 0)], [10, (0, 12.947348799736758, 0)], [11, (0, 17.770756750321226, 0)], [4, (0, 0, 31.631200681214498)], [13, (0, 22.594164700905694, 0)], [9, (0, 27.41757265149016, 0)], [7, (23.4425176663484, 0, 0)], [5, (0, 42.46081819666966, 0)], [6, (0, 47.28422614725413, 0)], [8, (0, 52.1076340978386, 0)]]
for i in range(len(points)):
    positions.append((points[i][1][0],
                     points[i][1][1],
                     points[i][1][2]))
    sizes.append((instances.iloc[points[i][0], 1], instances.iloc[points[i][0], 2], instances.iloc[points[i][0], 3]))
colors = ["limegreen" for i in range(len(points))]
print(positions)
print(sizes)
print(colors)

fig = plt.figure()
ax = fig.gca(projection='3d')

pc = plotCubeAt2(positions, sizes, colors=colors, edgecolor="k")
ax.add_collection3d(pc)

ax.set_xlim([0, 50])
ax.set_ylim([0, 60])
ax.set_zlim([0, 60])

plt.show()
