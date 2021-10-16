# list = []
# for j in range(4):
#     for i in range(6):
#         if j <= i % 6 <= j+3-1:
#             list = list + [1]
#         else:
#             list = list + [-1]

Tlist= []
d = 4
for i in range(d):
    list = []
    for k in range(d):
        if k != i :
            list.append(k + i*d)
            list.append(k*d + i)
    list = sorted(list)
    Tlist = Tlist + list