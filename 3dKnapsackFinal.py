# This programme aims at solving 3d knapsack problem using Simulated Annealing Algorithm
# Author: Can Zhang
# Date: May 18
import os

import pandas as pd
import numpy as np
import random
import math
import sys
import copy
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
import matplotlib.pyplot as plt
import matplotlib
sys.path.append("../")

matplotlib.use('TkAgg')
# # Loading instances
# instance = pd.read_csv(r"SampleInstances.csv")

# Setting the size of the container

con_w = 50
con_h = 60
con_l = 50


def calc_main(instances):
    # The objective function, return the object value for any given triple sequences
    def obj_fun(A, B, C):
        pos = []
        obpos = []

        def findpos(i1, i2):
            posF = 0
            # if reverseA,B,C, posF = 1 as top
            if list(A[::-1]).index(B[i1]) < list(A[::-1]).index(B[i2]) and list(C).index(B[i1]) < list(C).index(B[i2]):
                posF = 1
            # if A,B,reverseC, posF = 2 as right
            elif list(A).index(B[i1]) < list(A).index(B[i2]) and list(C[::-1]).index(B[i1]) < list(C[::-1]).index(
                    B[i2]):
                posF = 2
            # if reverseA,B,reverseC, posF = 3 as front
            elif list(A[::-1]).index(B[i1]) < list(A[::-1]).index(B[i2]) and list(C[::-1]).index(B[i1]) < list(
                    C[::-1]).index(B[i2]):
                posF = 3
            # if A,B,C, posF = 3 as front
            elif list(A).index(B[i1]) < list(A).index(B[i2]) and list(C).index(B[i1]) < list(C).index(B[i2]):
                posF = 3

            return posF

        for i in range(len(B)):
            minx = 0
            miny = 0
            minz = 0
            if i == 0:
                pos.append([B[i], (0, 0, 0)])
            else:

                for j in range(i):
                    posFlag = findpos(j, i)
                    if posFlag == 1:
                        if pos[j][1][1] + instances.iloc[B[j]][2] > miny:
                            miny = pos[j][1][1] + instances.iloc[B[j]][2]
                    elif posFlag == 2:
                        if pos[j][1][0] + instances.iloc[B[j]][1] > minx:
                            minx = pos[j][1][0] + instances.iloc[B[j]][1]
                    elif posFlag == 3:
                        if pos[j][1][2] + instances.iloc[B[j]][3] > minz:
                            minz = pos[j][1][2] + instances.iloc[B[j]][3]

                pos.append([B[i], (minx, miny, minz)])

        obj = 0
        for i in range(len(pos)):
            # width less than the limit and height less than the limit and length less than the limit
            if pos[i][1][0] + instances.iloc[pos[i][0], 1] < con_w and pos[i][1][1] + instances.iloc[
                pos[i][0], 2] < con_h and pos[i][1][2] + instances.iloc[pos[i][0], 3] < con_l:
                obj += 1
                obpos.append([pos[i][0], pos[i][1]])

        return obj, obpos

    # The new sequence generating function, given a known sequence A, B, C and return the new sequence A´,B´,C´
    # according to 5 rules, the probability of choosing any rule is the same
    def new_seq(A, B, C):
        p = random.uniform(0, 1)
        if 0 < p <= 0.2:
            flag = random.randint(0, 2)
            if flag == 0:
                index1, index2 = np.random.choice(len(A), 2, replace=False)
                A[index1], A[index2] = A[index2], A[index1]
            elif flag == 1:
                index1, index2 = np.random.choice(len(B), 2, replace=False)
                B[index1], B[index2] = B[index2], B[index1]
            elif flag == 2:
                index1, index2 = np.random.choice(len(C), 2, replace=False)
                C[index1], C[index2] = C[index2], C[index1]
        elif 0.2 < p <= 0.4:
            index1, index2 = np.random.choice(len(A), 2, replace=False)
            A[index1], A[index2] = A[index2], A[index1]
            index1, index2 = np.random.choice(len(B), 2, replace=False)
            B[index1], B[index2] = B[index2], B[index1]
        elif 0.4 < p <= 0.6:
            index1, index2 = np.random.choice(len(A), 2, replace=False)
            A[index1], A[index2] = A[index2], A[index1]
            index1, index2 = np.random.choice(len(C), 2, replace=False)
            C[index1], C[index2] = C[index2], C[index1]
        elif 0.6 < p <= 0.8:
            index1, index2 = np.random.choice(len(B), 2, replace=False)
            B[index1], B[index2] = B[index2], B[index1]
            index1, index2 = np.random.choice(len(C), 2, replace=False)
            C[index1], C[index2] = C[index2], C[index1]
        elif 0.8 < p <= 1:
            index1, index2 = np.random.choice(len(A), 2, replace=False)
            A[index1], A[index2] = A[index2], A[index1]
            index1, index2 = np.random.choice(len(B), 2, replace=False)
            B[index1], B[index2] = B[index2], B[index1]
            index1, index2 = np.random.choice(len(C), 2, replace=False)
            C[index1], C[index2] = C[index2], C[index1]
        return A, B, C

    # main loop of Simulated Annealing
    # number of iteration
    ite_num = 5000

    # choose initial incumbent solution s
    A = np.random.permutation(len(instances))
    B = np.random.permutation(len(instances))
    C = np.random.permutation(len(instances))

    # choose initial time t0
    t0 = 0.002
    # choose time step ts
    ts = 0.07
    # number of accepted solutions
    a = 0
    # rotation rate
    rot = 0.9

    # objectives
    objs = []

    for i in range(ite_num):

        # rotation rate change
        rotcr = (0.001 / rot) ** (1 / ite_num)
        rotRate = rot * rotcr ** (i - 1)
        # rotRate = 0.2
        # current objective value
        ObjS, pos = obj_fun(A, B, C)
        objs.append(ObjS)

        cA = copy.deepcopy(A)
        cB = copy.deepcopy(B)
        cC = copy.deepcopy(C)
        # accept rate
        acpt = 0
        # rotation or not
        r = random.uniform(0, 1)
        if 0 < r <= rotRate:
            rotIndex = np.random.choice(len(instances))
            rotFlag = np.random.choice(3) + 1

            # keep width
            if rotFlag == 1:
                instances.iloc[rotIndex, 2], instances.iloc[rotIndex, 3] = instances.iloc[rotIndex, 3], instances.iloc[
                    rotIndex, 2]
            # keep height
            elif rotFlag == 2:
                instances.iloc[rotIndex, 1], instances.iloc[rotIndex, 3] = instances.iloc[rotIndex, 3], instances.iloc[
                    rotIndex, 1]
            # keep length
            elif rotFlag == 3:
                instances.iloc[rotIndex, 1], instances.iloc[rotIndex, 2] = instances.iloc[rotIndex, 2], instances.iloc[
                    rotIndex, 1]

            new_A = A
            new_B = B
            new_C = C
        else:
            new_A, new_B, new_C = new_seq(cA, cB, cC)
            rotFlag = None

        ObjN, posN = obj_fun(new_A, new_B, new_C)

        accept = False

        if ObjN >= ObjS:
            accept = True
        elif ObjN < ObjS:
            if i / ite_num < 0.2:
                p = random.uniform(0, 1)
                T = 1 / (t0 + ts * a)
                delta = (ObjS - ObjN) / ObjS
                acpt = math.e ** (-delta / T)
                if p < math.e ** (-delta / T):
                    accept = True
        if accept:
            if not rotFlag:
                A, B, C = new_A, new_B, new_C
            a += 1
        elif not accept:
            if rotFlag == 1:
                instances.iloc[rotIndex, 2], instances.iloc[rotIndex, 3] = instances.iloc[rotIndex, 3], instances.iloc[
                    rotIndex, 2]
            elif rotFlag == 2:
                instances.iloc[rotIndex, 1], instances.iloc[rotIndex, 3] = instances.iloc[rotIndex, 3], instances.iloc[
                    rotIndex, 1]
            elif rotFlag == 3:
                instances.iloc[rotIndex, 1], instances.iloc[rotIndex, 2] = instances.iloc[rotIndex, 2], instances.iloc[
                    rotIndex, 1]
        sys.stdout.write(
            '\r' + "progress" + " " + str(round((i + 1) / ite_num * 100, 2)) + "%" + " " + "objective is " + str(
                ObjS) + " rotation rate is " + str(rotRate) + " accept prob is " + str(acpt))
        if len(pos) == len(instances):
            break

    print('\n')
    print("best triple sequences:", A, B, C)
    print("Maximum loading numbers:", ObjS)
    print("Position of these packages:", pos)

    return pos





def save_data(resultdf, i):
    resultdf['Volume'] = resultdf['Width'] * resultdf['Length'] * resultdf['Height']
    csv_name = detination_id + "_" + str(i) + ".csv"
    instancePath = os.path.join(data_path, str(i), csv_name)
    resultdf.to_csv(instancePath)
    print("Data saved succesfully")


def save_fig(instances, points,j):
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

    positions = []
    sizes = []

    for i in range(len(points)):
        positions.append((points[i][1][0],
                          points[i][1][1],
                          points[i][1][2]))
        sizes.append(
            (instances.iloc[points[i][0], 1], instances.iloc[points[i][0], 2], instances.iloc[points[i][0], 3]))
    colors = ["limegreen" for i in range(len(points))]

    fig = plt.figure()
    ax = fig.gca(projection='3d')

    pc = plotCubeAt2(positions, sizes, colors=colors, edgecolor="k")
    ax.add_collection3d(pc)

    ax.set_xlim([0, 50])
    ax.set_ylim([0, 60])
    ax.set_zlim([0, 50])

    fig_name = detination_id + "_" + str(j) + ".png"

    path = os.path.join(data_path, str(j), fig_name)
    plt.savefig(path, format="png", dpi=300)


if __name__ == '__main__':
    sheet_number = 27
    project_root_dir = "."
    seperate_volume = 13
    def load_for_one_des(instance):
        os.makedirs(data_path, exist_ok=True)
        j = 0
        while True:
            j += 1
            posIndex = []
            if len(instance) > seperate_volume:
                select_index = np.random.permutation(seperate_volume)

                select_ins = instance.iloc[select_index,].reset_index(drop=True)

                instance = instance.iloc[list(set(list(instance.index))-set(list(select_index))),].reset_index(drop=True)
                posFixed = calc_main(select_ins)
                for i in range(len(posFixed)):
                    posIndex.append(posFixed[i][0])
                if len(posIndex) == 0:
                    print('No selected 13 packages can be loaded!')
                    break
                os.makedirs(os.path.join(data_path, str(j)), exist_ok=True)
                save_data(select_ins.iloc[posIndex,], j)
                save_fig(select_ins, posFixed, j)
                left_ins = select_ins.iloc[list(set(list(select_ins.index)) - set(posIndex)),].reset_index(drop=True)
                if len(left_ins)>0:
                    instance = instance.append(left_ins).reset_index(drop=True)
            else:

                posFixed = calc_main(instance)
                for i in range(len(posFixed)):
                    posIndex.append(posFixed[i][0])
                if len(posIndex) == 0:
                    print('No selected packages can be loaded!')
                    break
                os.makedirs(os.path.join(data_path, str(j)), exist_ok=True)
                save_data(instance.iloc[posIndex,], j)
                save_fig(instance, posFixed, j)
                instance = instance.iloc[list(set(list(instance.index)) - set(posIndex)),].reset_index(drop=True)
                print(instance)
                if len(instance) <= 1:
                    break

    for k in range(18,sheet_number):
        detination_id = str(k)
        data_path = os.path.join(project_root_dir, detination_id)
        instance = pd.read_excel(r"./TConeyear_bydestination.xlsx",sheet_name = str(k+1),usecols=[0,2,3,4])
        load_for_one_des(instance)


    # pos = calc_main(instance)

# # plot figure



