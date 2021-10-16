# author: can zhang
# date: 2021/6/1

from pyLindo import *
import os
import pandas as pd
import networkx
import math
import matplotlib.pyplot as plt
import numpy as np
import sys
def logFunc(pModel,line,udict):
    print("%s" % (line),end=" ")
    sys.stdout.flush()

def cbFunc(pModel,iloc, udict):
    dObj = N.array([-1.0],dtype=N.double)
    errorcode = lindo.pyLSgetProgressInfo(pModel,iloc,LSconst.LS_DINFO_CUR_OBJ,dObj)
    dIter = N.array([-1.0],dtype=N.double)
    errorcode = lindo.pyLSgetProgressInfo(pModel,iloc,LSconst.LS_DINFO_CUR_ITER,dIter)
    print("\ncbFunc| LOC:%d Iter:%g, Obj=%g" % (iloc,dIter[0],dObj[0]),end =" ")
    sys.stdout.flush()

def cbMIPFunc(pModel,udict,dObj,padPrimal):
    dIter = N.array([-1.0],dtype=N.double)
    errorcode = lindo.pyLSgetProgressInfo(pModel,0,LSconst.LS_DINFO_CUR_ITER,dIter)
    print("\ncbMIPFunc| Iter:%g, Obj=%g" % (dIter[0],dObj,),end =" ")
    for k,x in enumerate(padPrimal):
        print("\n%.5f %s" % (padPrimal[k],udict["varType"][k]),end=" ")
    sys.stdout.flush()







# Model data
# load distance matrix

disMat  = pd.read_csv(r'C:\Lindoapi\python\pyLindo\MasterTHESIS\distance_matrix.csv')
## reshpe it to Adjacency Matrix

edgeList = disMat.values.tolist()
G = networkx.Graph()
for i in range(len(edgeList)):
    G.add_edge(edgeList[i][0],edgeList[i][1],weight = edgeList[i][2])
A = networkx.adjacency_matrix(G).A

# matrix for pacakges
B = pd.read_csv(r"C:\Lindoapi\python\pyLindo\MasterTHESIS\packages.csv").fillna(0).iloc[:,:-1]
B = pd.DataFrame(B,dtype=np.int8)
# number of pacakages
p = len(B)
# networkx.draw(G)
# plt.show()
# routes number k
k = 20
# capacity of the robot
m = 5
# number of destination selected
des_num = 9
A = A[:des_num]
# number of constrains
nCons = (des_num * 3 + p * des_num + 2) * k + p
print('number of constrains:', nCons)

# number of decision variables
nVars = (des_num * des_num  + des_num + p) * k
print('numher of decision variables:',nVars)
# 1; minimization type model and 0: maximization
nDir = 1

# A constant value to be added to the objective value
dObjConst = 0.0

# coefficent vector of objective functions
adC_list = []
for i in range(len(A)):
    for j in range(len(A)):
        adC_list.append(A[i][j])

for i in range(len(A)):
    adC_list.append(0)

for j in range(p):
    adC_list.append(0)

adC_list = adC_list * k
adC = N.array(adC_list,dtype=N.double)
#print('目标函数系数',adC)

# A double vector containing the constraint right-hand side coefficients.
adB_list = []

adB_list_1 = [0 for i in range(3 * des_num)] + [1]
adB_list_2 = []
for i in range(len(A)):
    adB_list_2.append(list(B.iloc[:,i]-1))
adB_list_2 = [i for item in adB_list_2 for i in item] + [m]

adB_list_3 = [1 for i in range(p)]
adB_list = k*(adB_list_1 + adB_list_2) + adB_list_3


adB = N.array(adB_list,dtype=N.double)
#print('约束右边系数总数为;',len(adB_list))
# print('约束右边的系数为',adB)


# A character vector containing the type of each constraint.

acContype_list_1 = ['E' for i in range(des_num*3+1)]
acContype_list_2 =['G' for i in range(p * des_num)] + ['L']
acConTypes_list_3 = ['E' for i in range(p)]
acContype_list = k * (acContype_list_1 + acContype_list_2) + acConTypes_list_3

#print('约束的数量为: ' ,len(acContype_list))
acConTypes = N.array(acContype_list,dtype=N.character)
#print('约束的符号为',acConTypes)

# the number of nonzero coefficients in the constraint matrix
nNZ = (des_num * des_num * 2 + des_num * 3 + 1 + 2 * p *des_num + p) * k + p * k
#print('约束中非0项的系数数量;',nNZ)
# Index of none zero value of columes array
ColList_des = [2 * x + math.ceil(x/(des_num + 1))  for x in range(des_num  * des_num )]
ColList_y0 = [2 * des_num**2 + des_num]
ColList_y = [ 2 * des_num**2 + des_num + 1 + (2 + p) * x  for x in range(1,des_num)]
r1_index = 2 * des_num**2 + des_num + 1 + (2 + p) * (des_num)
ColList_r = [r1_index + (des_num + 2) * x for x in range(p)]

dis = r1_index + (des_num + 2) * p

ColList = ColList_des + ColList_y0 + ColList_y + ColList_r
NoFirstlist = []
for i in range(1,k):
    NoFirstlist = NoFirstlist + [j + dis * i for j in ColList]
ColList = ColList + NoFirstlist

ColList.append(nNZ)

anBegCol = N.array(ColList,dtype=N.int32)
#print('none zero index:' ,ColList)
pnLenCol = N.asarray(None)
#print('非0项系数的index:',anBegCol)
# None zero coeffients in constrains
des_Cor = [1 for x in range(2*des_num*des_num + des_num)]
y0_Cor = [-1,-1,1] + [1 for i in range(p)]
y_Cor = ([-1,-1] + [1 for i in range(p)]) * (des_num - 1)
r_Cor =([-1 for i in range(des_num)] + [1,1]) * p
adA_list = (des_Cor + y0_Cor + y_Cor + r_Cor) * k
adA = N.array(adA_list,dtype=N.double)
print('None zero coeffients:', len(adA_list))
print('None zero cof list ',adA)
# the row index of those none zero coeffients
a_row = []
y_row = []



for i in  range(des_num*des_num):
    if i % (des_num + 1) == 0:
        a_row = a_row + [int(i/des_num),int((i % des_num) + des_num)] + [int(2*des_num + i/(des_num + 1))]
    else:
        a_row = a_row + [int(i/des_num),int((i % des_num) + des_num)]
for i in range(des_num):
    if i == 0:
        y_row = [0,des_num,3 * des_num] + [3 * des_num + x +1 for x in range(p)]
    else:
        y_row = y_row + [i ,i  + des_num] + [3 * des_num + p*i + x +1 for x in range(p)]
# for i in range(p):
#     r_row = r_row + [3 * des_num + 1 + i + p * x for x in range(des_num)] + [3 * des_num + p * des_num + 1] + [(k-1) * (3 *des_num + p * des_num + 1) + k - 1 + i]

anRowX_list = []

for j in range(k):
    anRowX_list = anRowX_list + [i + j * (3 * des_num + p * des_num + 2) for i in  a_row] \
                  + [i + j * (3 * des_num + p * des_num + 2) for i in  y_row]
    r_row = []
    for l in range(p):
        r_row = r_row + [3 * des_num + 1 + l + p * x +j * (3 * des_num + p * des_num + 2) for x in range(des_num)] + \
                [3 * des_num + p * des_num + 1 + j * (3 * des_num + p * des_num + 2)] + [
            k  * (3 * des_num + p * des_num + 1) + k  + l]

    anRowX_list = anRowX_list + r_row

anRowX = N.array(anRowX_list,dtype=N.int32)
print('row index',len(anRowX_list))
print('row_index list',anRowX)
print(len(anRowX))
# Lower bound of variables
lb_list = [0 for x in range(nVars)]
pdLower = N.array(lb_list,dtype=N.double)

# Upper bound of variables
ub_list = [1 for x in range(nVars)]
pdUpper = N.array(ub_list,dtype=N.double)

print(len(lb_list),len(ub_list))
print(pdLower,pdUpper)

# Variable types, in this cases, they have been set to Binary variables
VarType_list = ['B'  for x in range(nVars)]

#print('Binary 变量的数量为: ' ,len(VarType_list))

pachVarType = N.array(VarType_list,dtype=N.character)
#print('Binary 变量为', pachVarType)
# create LINDO environment and model objects
LicenseKey = N.array('', dtype='S1024')
lindo.pyLSloadLicenseString(os.getenv('LINDOAPI_HOME') + '/license/lndapi130.lic', LicenseKey)

pnErrorCode = N.array([-1], dtype=N.int32)

pEnv = lindo.pyLScreateEnv(pnErrorCode, LicenseKey)
geterrormessage(pEnv, pnErrorCode[0])

verstr = getversion(pEnv)

pModel = lindo.pyLScreateModel(pEnv, pnErrorCode)
geterrormessage(pEnv, pnErrorCode[0])

# load data into the model
print("Loading LP data...")
errorcode = lindo.pyLSloadLPData(pModel, nCons, nVars, nDir,
                                 dObjConst, adC, adB, acConTypes, nNZ, anBegCol,
                                 pnLenCol, adA, anRowX, pdLower, pdUpper)
geterrormessage(pEnv, errorcode)

errorcode = lindo.pyLSloadVarType(pModel, pachVarType)
geterrormessage(pEnv, errorcode)

udict = {
    "prefix": "APILOG",
    "postfix": "..",
    "version": verstr,
    "varType": pachVarType
}
errorcode = lindo.pyLSsetModelLogfunc(pModel, logFunc, udict)
geterrormessage(pEnv, errorcode)

# errorcode = lindo.pyLSsetCallback(pModel,cbFunc, udict)
# geterrormessage(pEnv,errorcode)

errorcode = lindo.pyLSsetMIPCallback(pModel, cbMIPFunc, udict)
geterrormessage(pEnv, errorcode)

# solve the model
print("Solving the model...")
pnStatus = N.array([-1], dtype=N.int32)
errorcode = lindo.pyLSsolveMIP(pModel, pnStatus)
geterrormessage(pEnv, errorcode)

# retrieve the objective value
dObj = N.array([-1.0], dtype=N.double)
errorcode = lindo.pyLSgetInfo(pModel, LSconst.LS_DINFO_MIP_OBJ, dObj)
geterrormessage(pEnv, errorcode)
print("Objective is: %.5f" % dObj[0])
print("")

# retrieve the primal solution
varType = N.empty((nVars), dtype=N.byte)
errorcode = lindo.pyLSgetVarType(pModel, varType)
geterrormessage(pEnv, errorcode)

padPrimal = N.empty((nVars), dtype=N.double)
errorcode = lindo.pyLSgetMIPPrimalSolution(pModel, padPrimal)
geterrormessage(pEnv, errorcode)
print("Primal solution is: ")
for k, x in enumerate(padPrimal):
    print("%.5f %c" % (padPrimal[k], varType[k]))

# delete LINDO model pointer
errorcode = lindo.pyLSdeleteModel(pModel)
geterrormessage(pEnv, errorcode)

# delete LINDO environment pointer
errorcode = lindo.pyLSdeleteEnv(pEnv)
geterrormessage(pEnv, errorcode)


