import matplotlib as mpl
import matplotlib.pyplot as plt
import networkx as nx
import numpy as np
import os
import pandas as pd

disMat  = pd.read_csv(r'C:\Lindoapi\python\pyLindo\MasterTHESIS\distance_matrix_case_study.csv')
des_num = 6
## reshpe it to Adjacency Matrix

edgeList = disMat.values.tolist()
G = nx.Graph()
for i in range(len(edgeList)):
    G.add_edge(edgeList[i][0],edgeList[i][1],weight = edgeList[i][2])
A = nx.adjacency_matrix(G).A
A = np.c_[A,A[:,0]]
A = np.r_[A,np.array([A[0,:]])]
A = np.c_[A[:,:des_num],A[:,-1]]
A = np.r_[A[:des_num],np.array([A[-1,:]])]





des_list = ['CTC', 'ACE', 'Biblioteket','E-huset','Fysik', 'Kemi','Maskin', 'SB3',
           'Vasa','CTC']
des_list_test = ['CTC', 'ACE', 'Biblioteket','CTC']

file = open("result3.txt")
lines = file.readlines()[0]
lines = lines.strip('[').strip(']')
paralist = [int(float(i)) for i in lines.split(",")]
print(len(paralist))
#paralist = lines[0]


des_num = 7
p = 30
_k = 10
nVars = (des_num * des_num  + 2*des_num + p) * _k
# _k = 20
total_dis = 0


def draw_and_save_graph(paralist, save_path,des_list,pkg_indx):
    lst = []
    paralist = np.array(paralist).reshape((des_num,des_num))
    des_list = des_list[:des_num-1] + ["CTC"]
    for i in range(des_num):
        for j in range(des_num):
            if paralist[i][j]:
                lst.append((des_list[i], des_list[j]))
                global total_dis
                total_dis += A[i][j]
                print(A[i][j])
                print((des_list[i], des_list[j]))
    G = nx.DiGraph()
    G.add_edges_from(lst)
    pos = {'CTC': np.array([-1., 1.]), 'ACE': np.array([-0.3, 0.5308867]),
           'Biblioteket': np.array([0.50013948, 0.51397026]),
           'E-huset': np.array([-0.4878867, -0.02045415]),
           'Fysik': np.array([0.1993731, -0.35215071]), 'Kemi': np.array([-0.56871152, -1.]),
           'Maskin': np.array([-0.00279408, 0.005261577]), 'SB3': np.array([0.93920119, 0.55470652]),
           'Vasa': np.array([0.93920119, -0.75470652])}

    # pos = nx.layout.spring_layout(G)
    print(pos)
    # nodes = nx.draw_networkx_nodes(G, pos, node_size = 650,node_color="blue")
    nx.draw_networkx_nodes(G, pos, nodelist=des_list, node_size=650, node_color="blue")
    edges = nx.draw_networkx_edges(
        G,
        pos,
        arrows=True,
        arrowstyle="->",
        arrowsize=20,
        edge_color="black",
        width=2,
    )

    nx.draw_networkx_labels(G, pos, labels={k: k for k in pos.keys()}, font_size=7, font_color='white')
    plt.title("The selected packages in this route is {}".format(str(pkg_indx)))

    plt.savefig(save_path)
    plt.show()
ind = 0
package_index = [_ for _ in range(p)]
for i in range(10):
    r_list = paralist[i*nVars//_k:nVars//_k + i*nVars//_k][-p:]
    if 1 in r_list:
        ind += 1
        pdlist = paralist[i * nVars // _k:nVars // _k + i * nVars // _k][:des_num**2]

        pkg_index_list = [l for l,m in enumerate(r_list) if m == 1]
        save_path_root  = os.path.join("./pic",str(ind)+'.jpg')
        draw_and_save_graph(pdlist, save_path_root, des_list,pkg_index_list)

print(total_dis)
# lst = []
# para_list = np.array([0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0]).reshape((4,4))
# for i in range(4):
#     for j in range(4):
#         if para_list[i][j]:
#             lst.append((des_list_test[i],des_list_test[j]))
#
# G = nx.DiGraph()
# G.add_edges_from(lst)
# pos = {'CTC': np.array([-1., 1.]), 'ACE': np.array([-0.5, 0.5308867]),
#        'Biblioteket': np.array([0.50013948, 0.51397026]),
#        'E-huset': np.array([-0.4878867, -0.02045415]),
#        'Fysik': np.array([0.1993731, -0.35215071]), 'Kemi': np.array([-0.56871152, -1.]),
#        'Maskin': np.array([-0.00279408, 0.005261577]), 'SB3': np.array([0.93920119, 0.55470652]),
#        'Vasa': np.array([0.93920119, -0.75470652])}
#
# # pos = nx.layout.spring_layout(G)
# print(pos)
# #nodes = nx.draw_networkx_nodes(G, pos, node_size = 650,node_color="blue")
# nx.draw_networkx_nodes(G, pos, nodelist=des_list,node_size = 650,node_color="blue")
# edges = nx.draw_networkx_edges(
#     G,
#     pos,
#     arrows=True,
#     arrowstyle="->",
#     arrowsize=20,
#     edge_color="black",
#     width=2,
# )
#
# nx.draw_networkx_labels(G, pos,labels={k:k for k in pos.keys()},font_size=7,font_color='white')
# plt.show()