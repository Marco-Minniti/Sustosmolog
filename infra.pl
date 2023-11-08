:-dynamic request/5.
request(arApp, adaptive, full, 999).

:-dynamic node/4.
node(node0,[(gcc,1),(caffe,1)],(32, 0),[(phone,2),(lightSensor,1)]).
node(node1,[(gcc,2),(make,2),(docker,2)],(64, 3),[]).
node(node2,[(docker,2)],(64, 2),[]).
node(node3,[(docker,2)],(64, 1),[]).
node(node4,[(docker,1)],(32, 3),[]).
node(node5,[(docker,1)],(32, 3),[]).
node(node6,[(docker,2)],(32, 1),[(phone,2),(lightSensor,1)]).
node(node7,[(gcc,1),(caffe,1)],(32, 1),[]).
node(node8,[(docker,1)],(64, 2),[(phone,5),(lightSensor,2)]).
node(node9,[(gcc,1),(caffe,1)],(64, 2),[]).
:-dynamic link/3.
link(node0,node1,60.0).
link(node0,node2,120.0).
link(node0,node3,10).
link(node0,node4,50).
link(node0,node5,10).
link(node0,node6,150).
link(node0,node7,150).
link(node0,node8,25).
link(node0,node9,25).
link(node1,node5,60.0).
link(node1,node6,150).
link(node1,node7,10).
link(node2,node5,180.0).
link(node2,node7,50).
link(node3,node5,5).
link(node3,node6,25).
link(node3,node7,150).
link(node3,node8,25).
link(node5,node6,25).
link(node5,node8,150).
link(node6,node9,12.0).
link(node7,node8,150).
link(node7,node9,150).
link(node8,node9,150).
:-dynamic energyConsumption/2.
energyConsumption(node0,4).
energyConsumption(node1,3).
energyConsumption(node2,1).
energyConsumption(node3,3).
energyConsumption(node4,3).
energyConsumption(node5,2).
energyConsumption(node6,4).
energyConsumption(node7,3).
energyConsumption(node8,1).
energyConsumption(node9,2).
:-dynamic energyMix/2.
energyMix(node0,[(coal, 0.7), (gas, 0.1), (solar, 0.2)]).
energyMix(node1,[(coal, 0.7), (gas, 0.1), (solar, 0.2)]).
energyMix(node2,[(coal, 0.3), (gasoline, 0.7)]).
energyMix(node3,[(coal, 0.7), (gas, 0.1), (solar, 0.2)]).
energyMix(node4,[(coal, 0.3), (gasoline, 0.7)]).
energyMix(node5,[(coal, 0.3), (gasoline, 0.7)]).
energyMix(node6,[(coal, 0.1), (solar, 0.9)]).
energyMix(node7,[(coal, 0.3), (gasoline, 0.7)]).
energyMix(node8,[(coal, 0.3), (gasoline, 0.7)]).
energyMix(node9,[(coal, 0.3), (gasoline, 0.7)]).
