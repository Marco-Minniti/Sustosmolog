import networkx as nx
import math
import random as rnd
from pyswip import Prolog
import time

def findSolutionExhaustive():
    prolog = Prolog()
    prolog.consult("fullRanking.pl")
    sols = list(prolog.query("go((0,highest), arApp, adaptive, full, 110, VC, C, Best, Time)."))
    return sols

def findSolutionHeuristic():
    prolog = Prolog()
    prolog.consult("greedy.pl")
    sols = list(prolog.query("go(arApp, adaptive, full, 110, Best, VC, C, Time)"))
    return sols

def generateInfrastructure(seed,n,m):
        
    G = nx.generators.random_graphs.barabasi_albert_graph(n,m,seed)

    for i in range(0,n):
        iot = rnd.random() > 0.9 # 10% of the nodes reaches out the iot devices involved
        edge = rnd.random() > 0.2 # 80% of the nodes in the edge, 20% in the cloud

        if (edge):
            G.nodes[i]['hardware'] = str((rnd.choice([2,4,8,16,32]),rnd.choice(range(0,8))))
        else:
            G.nodes[i]['hardware'] = str((rnd.choice([64,128,256]),rnd.choice(range(0,8))))

        G.nodes[i]['software'] = rnd.choice( [\
        '[(docker,1)]','[(docker,2)]','[(docker,3)]',\
            '[(gcc,1),(make,1)]', '[(gcc,2),(make,2)]', '[(gcc,3),(make,3)]',\
                '[(gcc,1),(caffe,1)]','[(gcc,2),(caffe,2)]','[(gcc,3),(caffe,3)]',\
                    '[(docker,1),(gcc,1),(caffe,1)]', '[(docker,2),(gcc,2),(caffe,2)]', '[(docker,3),(gcc,3),(caffe,3)]',\
                        '[(gcc,1)]', '[(gcc,2)]', '[(gcc,3)]'\
        ])

        if (iot):
            G.nodes[i]['iot'] = rnd.choice( [\
                '[(phone,5),(lightSensor,2)]',\
                '[(phone,2),(lightSensor,1)]',\
                '[(phone,3)]',\
                '[(lightSensor,1)]'\
            ])
        else:
            G.nodes[i]['iot'] = rnd.choice(['[(s1,1),(s2,1),(s3,1)]', '[]'])


    for (i,j) in G.edges():
        G.edges[i,j]['latency'] = rnd.choice([5,10,25,50,100,150])

    f = open("infra.pl","w+")
    f.write(':-dynamic node/4.\n')
    for i in range(0,n):
        node = G.nodes[i]
        newnode = 'node(node'+str(i)+','+node['software']+','+node['hardware']+','+node['iot']+').\n'
        f.write(newnode)
    for (i,j) in G.edges():
        link=G.edges[i,j]
        newlink='link(node'+str(i)+',node'+str(j)+','+str(link['latency'])+').\n'
        f.write(newlink)

    f.close()


seed = 481183
RUNS = 20
MAX = 9
rnd.seed(seed)

for n in [2**x for x in range(3,MAX,1)]:
    exhaustive = {
        'time' : 0,
        'cost' : 0,
        'vc'   : 0,
        'fails'  : 0
    }
    heuristic = {
        'time' : 0,
        'cost' : 0,
        'vc'   : 0,
        'fails'  : 0
    }
    m=int(math.log2(n))
    run=0
    while run < RUNS:
        seed = seed + 1
        rnd.seed(seed)
        generateInfrastructure(seed,n,m)
        
        exhaustiveSols = findSolutionExhaustive()
        heuristicSols = findSolutionHeuristic()

        if (exhaustiveSols != []):
            for sol in exhaustiveSols:
                p=sol["Best"]
                if (p!='none'):
                    exhaustive['cost'] = exhaustive['cost'] + sol["C"]
                    exhaustive['vc'] = exhaustive['vc'] + sol["VC"]
                    exhaustive['time'] = exhaustive['time'] + sol["Time"]
                    run = run + 1
                else:
                    exhaustive['fails'] = exhaustive['fails'] + 1
        else:
            exhaustive['fails'] = exhaustive['fails'] + 1

        if (heuristicSols != []):
            for sol in heuristicSols:
                p=sol["Best"]
                if (p!='none'):
                    heuristic['cost'] = heuristic['cost'] + sol["C"]
                    heuristic['vc'] = heuristic['vc'] + sol["VC"]
                    heuristic['time'] = heuristic['time'] + sol["Time"]
                else:
                    heuristic['fails'] = heuristic['fails'] + 1
        else:
            heuristic['fails'] = heuristic['fails'] + 1
    
    heuristic['cost'] = heuristic['cost']/RUNS
    heuristic['vc'] = heuristic['vc']/RUNS
    heuristic['time'] = heuristic['time']/RUNS

    exhaustive['cost'] = exhaustive['cost']/RUNS
    exhaustive['vc'] = exhaustive['vc']/RUNS
    exhaustive['time'] = exhaustive['time']/RUNS

    print("Nodes:",n)
    print("Exhaustive:\n", exhaustive)
    print("Greedy:\n", heuristic)

RUNS = 20
MAX = 9
rnd.seed(seed)

for n in [2**x for x in range(9,12,1)]:
    heuristic = {
        'time' : 0,
        'cost' : 0,
        'vc'   : 0,
        'fails'  : 0
    }
    m=int(math.log2(n))
    run=0
    while run < RUNS:
        seed = seed + 1
        rnd.seed(seed)
        generateInfrastructure(seed,n,m)

        heuristicSols = findSolutionHeuristic()

        if (heuristicSols != []):
            for sol in heuristicSols:
                p=sol["Best"]
                if (p!='none'):
                    heuristic['cost'] = heuristic['cost'] + sol["C"]
                    heuristic['vc'] = heuristic['vc'] + sol["VC"]
                    heuristic['time'] = heuristic['time'] + sol["Time"]
                    run = run + 1
                else:
                    heuristic['fails'] = heuristic['fails'] + 1
        else:
            heuristic['fails'] = heuristic['fails'] + 1
    
    heuristic['cost'] = heuristic['cost']/RUNS
    heuristic['vc'] = heuristic['vc']/RUNS
    heuristic['time'] = heuristic['time']/RUNS

    print("Nodes:",n)
    print("Greedy:\n", heuristic)


