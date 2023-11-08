# Install libraries: python3 -m pip install <Library>  

import networkx as nx # creare e manipolare grafi
import math
import random as rnd
import time # per misurare il tempo di esecuzione
import re # per operazioni di espressioni regolari
from swiplserver import PrologMQI, PrologThread, json_to_prolog
import shutil
# Devo:
# • Prendere tempi di esecuzioni
# • co2
# • Profitto
# E vedere come variano i valori (plot)
# Nella latenza, lanciare monetina, es aumentare di 100 in un caso oppure 20 nell'altro.


# def findSolutionHeuristic():
#     with PrologMQI() as mqi:
#             with mqi.create_thread() as prolog_thread:
#                 prolog_thread.query("[exhaustive]")
#                 result = prolog_thread.query("best()")
#                 # print(json_to_prolog(result))
#                 print(result)
#                 mqi.stop()




def generateInfrastructure(seed,n,m):
        
    G = nx.generators.random_graphs.barabasi_albert_graph(n,m,seed)
    # n = Numero di nodi, m = Numero di archi da collegare ai nuovi nodi, seed = Seme per la riproducibilità

    for i in range(0,n):
        iot = rnd.random() > 0.9 #  Valore booleano, True al 10%, False al 90%.
        edge = rnd.random() > 0.2 # Valore booleano, True al 80%, False al 20%.
        
        #if (edge):
        G.nodes[i]['hardware'] = str((rnd.choice([32,64]),rnd.choice(range(0,4))))
        # else:
        #     G.nodes[i]['hardware'] = str((rnd.choice([64,128,256]),rnd.choice(range(0,8))))

        # G.nodes[i]['software'] = rnd.choice( [\
        # '[(docker,1)]','[(docker,2)]','[(docker,3)]',\
        #     '[(gcc,1),(make,1)]', '[(gcc,2),(make,2)]', '[(gcc,3),(make,3)]',\
        #         '[(gcc,1),(caffe,1)]','[(gcc,2),(caffe,2)]','[(gcc,3),(caffe,3)]',\
        #             '[(docker,1),(gcc,1),(caffe,1)]', '[(docker,2),(gcc,2),(caffe,2)]', '[(docker,3),(gcc,3),(caffe,3)]',\
        #                 '[(gcc,1)]', '[(gcc,2)]', '[(gcc,3)]'\
        # ])
        G.nodes[i]['software'] = rnd.choice( [\
        '[(docker,1)]','[(docker,2)]',\
            '[(gcc,1),(make,1)]', '[(gcc,2),(make,2),(docker,2)]',\
                '[(gcc,1),(caffe,1)]','[(gcc,2),(caffe,2),(docker,2)]',\
                    #'[(docker,1),(gcc,1),(caffe,1)]', '[(docker,2),(gcc,2),(caffe,2)]',\
                        #'[(gcc,1)]', '[(gcc,2)]',\
        ])

        #if (iot):
        G.nodes[i]['iot'] = rnd.choice( [\
            '[(phone,5),(lightSensor,2)]',\
            '[(phone,2),(lightSensor,1)]',\
            #'[(phone,3)]',\
            #'[(lightSensor,1)]',\
            '[]',\
        ])
        # else:
        #     G.nodes[i]['iot'] = rnd.choice(['[(s1,1),(s2,1),(s3,1)]', '[]'])


        G.nodes[i]['energyConsumption'] = rnd.choice(range(1,5))
        G.nodes[i]['energyMix'] = rnd.choice( [\
                '[(coal, 0.7), (gas, 0.1), (solar, 0.2)]',\
                '[(coal, 0.3), (gasoline, 0.7)]',\
                '[(gasoline, 0.2), (onshorewind, 0.6), (offshorewind, 0.2)]',\
                '[(coal, 0.1), (solar, 0.9)]',\
            ])

    for (i,j) in G.edges():
        G.edges[i,j]['latency'] = rnd.choice([5,10,25,50,100,150])
    # for i in G.edges():
    #     for j in G.edges():
    #         if i != j:
    #             G.edges[i,j]['latency'] = rnd.choice([5,10,25,50,100,150])

    writeOnFile(G, n, False)
    return G
    
    

def writeOnFile(G, n, flag):
    f = open("infra.pl","w+")

    f.write(':-dynamic request/5.\n')
    requests = rnd.choice( [\
            'request( arApp, adaptive, 999).\n\n',\
            #'request( arApp, adaptive, 999).\nrequest( app1, adaptive, 999).\n\n',\
            #'request( arApp, adaptive, 999).\nrequest( app1, adaptive, 999).\nrequest( testApp, adaptive, 999).\n\n',\
            ])
    f.write(requests)

    f.write(':-dynamic node/4.\n')
    for i in range(0,n):
        node = G.nodes[i]
        p = rnd.random() > 0.1
        if (p and flag):
            newnode = 'node(node'+str(i)+','+node['software']+','+node['hardware']+','+node['iot']+').\n'
            f.write(newnode)
        if not flag:
            newnode = 'node(node'+str(i)+','+node['software']+','+node['hardware']+','+node['iot']+').\n'
            f.write(newnode)
    f.write(':-dynamic link/3.\n')
    for (i,j) in G.edges():
        link=G.edges[i,j]
        newlink='link(node'+str(i)+',node'+str(j)+','+str(link['latency'])+').\n'
        f.write(newlink)
    f.write(':-dynamic energyConsumption/2.\n')
    for i in range(0,n):
        node = G.nodes[i]
        nodeconsumption = 'energyConsumption(node'+str(i)+','+str(node['energyConsumption'])+').\n'
        f.write(nodeconsumption)
    f.write(':-dynamic energyMix/2.\n')
    for i in range(0,n):
        node = G.nodes[i]
        nodeconsumption = 'energyMix(node'+str(i)+','+str(node['energyMix'])+').\n'
        f.write(nodeconsumption)

    f.close()


def updateInfrastructure(G, n):
    for i in range(0,n):
        p = rnd.random() > 0.7
        if (p):
            G.nodes[i]['energyConsumption'] = rnd.choice(range(1,5))
            G.nodes[i]['energyMix'] = rnd.choice( [\
                    '[(coal, 0.7), (gas, 0.1), (solar, 0.2)]',\
                    '[(coal, 0.3), (gasoline, 0.7)]',\
                    '[(gasoline, 0.2), (onshorewind, 0.6), (offshorewind, 0.2)]',\
                    '[(coal, 0.1), (solar, 0.9)]',\
                ])
    for (i,j) in G.edges():
        link = rnd.random() > 0.8
        if (link):
            G.edges[i,j]['latency'] = G.edges[i,j]['latency']*1.20
    writeOnFile(G, n, True)
    
        
    



seed = 482180
n = 10
rnd.seed(seed)
RUNS=3
G = generateInfrastructure(seed,n,int(math.log2(20)))

run=0
with PrologMQI() as mqi:
        with mqi.create_thread() as prolog_thread:
            prolog_thread.query("[exhaustive]")
            while run < RUNS:
                prolog_thread.query("once(loadInfrastructure())")
                result = prolog_thread.query("once(best(Infos, Details))")
                shutil.copy('infra.pl', 'infras/'+ str(run))
                updateInfrastructure(G, n)
                # for solution in result:
                #     Infos = solution["Infos"]
                #     Details = solution["Details"]
                #     print("Details:", Details)

                run = run + 1
            mqi.stop()


