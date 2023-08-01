import numpy as np
import pandas as pd
import math
import matplotlib.pyplot as plt
import seaborn as sns
sns.set_theme(style="darkgrid")

nodes = [8,16,32,64,128,256,512,1024,2048]
exhaustive = [0.0015325500000000214,0.008855749999999319,0.05319975000000028,0.35068230000000045,4.461654249999999,58.27436894999998, 740.7038778, None, None]
heuristic = [0.0007171999999998402, 0.002048349999999566, 0.006740549999999956, 0.028384050000000372, 0.37272784999999475, 1.092796499999983, 11.05871479999995, 27.789899899999956, 140.42707494999996]

e_cost = [55.75,61.75,58.0,73.9,83.9,61.15]
e_vc   = [86.25,85.0,83.75,77.5,78.75,86.25]

h_cost = [50.30,63.45,62.8,86.1,76.85,76.4,93.6,96.8,102.7]
h_vc   = [77.5,81.25,81.25,72.5,70.0,71.25,67.5,75.0,65.0]

speedup = [2.136851645287734, 4.323357824591108, 7.8924939359548745, 12.354907069287005, 11.970273350918267, 53.32591104565295, 66.97920067529034, None, None]



fig, ax = plt.subplots()
#plt.xscale('log')
#plt.yscale('log')

plt.ylim((-5,100))
plt.xlim((-5, 600))

plt.xlabel('Nodes')
plt.ylabel('Execution Time [ms]')
plt.xticks([x for x in range(0,641,128)])
#ax.set_yticklabels([math.log(x) for x in range(100,801,100)])
ex_line,  = plt.plot(nodes, exhaustive, 'bo')
he_line,  = plt.plot(nodes, heuristic, 'g^')
speedup,   = plt.plot(nodes,speedup,'m+')


plt.legend(handles=[ex_line,he_line,speedup], labels=['Exhaustive', 'Heuristic','Speedup'], loc='upper right')

plt.show()


# # set width of bar 
# barWidth = 0.25
# fig = plt.subplots(figsize =(12, 12)) 
   
# # set height of bar 
# groups = [
#     [55.75,61.75,58.0,73.9,83.9,61.15,70.35], #ecost
#     [50.30,63.45,62.8,86.1,76.85,76.4,93.6], #hcost
#     [86.25,85.0,83.75,77.5,78.75,86.25,85.0], #evc
#     [77.5,81.25,81.25,72.5,70.0,71.25,67.5] #hvc
# ]
   
# n_groups = len([8,16,32,64,128,256,512])

# # create plot
# fig, ax = plt.subplots()
# index = np.arange(n_groups)
# margin=0.005
# width = (1.-2.*margin)/n_groups
# opacity = 0.8
# colors = ['forestgreen', 'lightgreen', 'royalblue', 'lightblue']
# labels = ['Optimal Cost', 'Heuristic Cost', 'Optimal VC', 'Heuristic VC']

# i = 0
# for num, vals in enumerate(groups):
#     xdata=index+margin+(num*width)
#     rects = plt.bar(xdata, vals, width, color=colors[i%4], alpha=opacity, label= labels[i%4])
#     i = i + 1

# plt.xlabel('Nodes')
# #plt.ylabel('Values')

# plt.xticks(index + 2*width-2*margin, (8,16,32,64,128,256,512))
# plt.legend(loc='upper center', bbox_to_anchor=(0.5, -0.12), ncol=4)

# plt.tight_layout()
# plt.show()





