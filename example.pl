%%%%%%%%%%%%%%%%%%%%%%%
% Example Application %
%%%%%%%%%%%%%%%%%%%%%%%
mel((usersData,full), [docker], 64, []).

mel((videoStorage,full), [docker], 16, []).
mel((videoStorage,medium), [docker], 8, []).

mel((movementProcessing,full), [docker], 8, []).
mel((movementProcessing,medium), [gcc, make], 4, []).

mel((arDriver,full), [docker], 4, [phone, lightSensor]).
mel((arDriver,medium), [gcc,caffe], 2, [phone, lightSensor]).
mel((arDriver,light), [gcc], 1, [phone]).

mel((mel1,medium), [docker], 9, []). %cloud42
mel((mel1,full), [docker], 2, []). %cloud42

mel((mel2,light), [gcc,caffe], 2, []). %edge42

mel((testMel,full), [docker], 20, []). %cloud42
mel((testMel2,full), [gcc,caffe], 5, []). %edge42

% CONTINUOUS REASONING TEST
mel((s1,full), [test], 4, []).
mel((s2,full), [test], 6, []).
% ------------------------------
% mel((s1,full), [test], 6, []).
% mel((s2,full), [test], 1, []).


mel2mel(usersData, videoStorage, 70).
mel2mel(videoStorage, movementProcessing, 30).
mel2mel(movementProcessing, arDriver, 20).
mel2mel(mel1, mel2, 999). % latenza massima supportata

application((arApp, full), [(usersData,full), (videoStorage,full), (movementProcessing,full), (arDriver,full)]).
application((arApp, adaptive), [(usersData,full), (videoStorage,_), (movementProcessing,_), (arDriver,_)]).
application((testApp, adaptive), [(testMel,full), (testMel2,full)]).
application((app1, adaptive), [(mel1,_), (mel2,_)]).
application((appCR, adaptive), [(s1,_), (s2,_)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%
% Example Infrastructure %
%%%%%%%%%%%%%%%%%%%%%%%%%%

node(edge42, [(gcc,0),(caffe,4)], (6, 3), [(phone,1),(lightSensor,1)]).
node(cloud42, [(docker, 5)], (100, 1), []).
node(n1, [(test,0)], (5, 1), []).
node(n2, [(test,0)], (7, 1), []).

% energyConsumption(NodeId, WattXHwUnit)
energyConsumption(edge42, 3).
energyConsumption(cloud42, 2).
energyConsumption(n1, 0).
energyConsumption(n2, 0).
energyConsumption(n8, 0).

% Format: energyMix( NodeId, [(Fonte fossile, % usata)], [(Fonte rinnovabile, % usata)] ). 
energyMix(edge42, [(carbone, 0.7), (petrolio, 0.1), (solare, 0.2)]).
energyMix(cloud42, [(petrolio, 0.6), (solare, 0.4)]).
energyMix(n1, []).
energyMix(n2, []).
energyMix(n8, []).

% Format: co2(Fonte, kgCO2-eq/kWh).
co2(carbone, 1.1).
co2(petrolio, 1.0).
co2(solare, 0.02).
co2(gas, 0.610).
co2(coal, 1.1).
co2(onshorewind, 0.0097).
co2(offshorewind, 0.0165).
co2(solar, 0.05). 
% https://solarbay.com.au/portfolio-item/how-much-emissions-does-solar-power-prevent/


target(energy, 2000).
target(co2, 1000).


link(edge42, cloud42, 20).
















%%%%%%%%%%%%%%%%%%%%%%%%%%
% INPUT / QUERY format: goForBest(SortType, AppId, AppVersion, PreferredMELVersion, MaxCost, BestPlacement).

% EXAMPLE:
%?- goForBest((0,highest), arApp, adaptive, full, 110, Best).
%---> BestPlacement = [175, 75, 107, [on(usersData, full, cloud42), on(videoStorage, full, cloud42), on(movementProcessing, full, cloud42), 
%                                    on(arDriver, light, edge42)]] 
%%%%%%%%%%%%%%%%%%%%%%%%%%

% Voglio che questo sia il placement selezionato da goforbest -> devo fare un app che non faccia entrare gli altri 2 placement 
% [on(usersData,full,cloud42),on(videoStorage,medium,cloud42),on(movementProcessing,full,cloud42),on(arDriver,light,edge42)],99,[(edge42,1),(cloud42,80)]
% Altri 2 da disabilitare 
% [on(usersData,full,cloud42),on(videoStorage,full,cloud42),on(movementProcessing,full,cloud42),on(arDriver,light,edge42)],107,[(edge42,1),(cloud42,88)]
% [on(usersData,full,cloud42),on(videoStorage,medium,cloud42),on(movementProcessing,full,cloud42),on(arDriver,medium,edge42)],107,[(edge42,2),(cloud42,80)]



