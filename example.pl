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

mel((mel3,full), [docker], 50, []). %cloud42
mel((mel3,medium), [docker], 20, []). %cloud42
mel((mel3,light), [docker], 5, []). %cloud42

mel((mel4,full), [gcc,caffe], 5, []). %edge42
mel((mel4,medium), [gcc,caffe], 4, []). %edge42
mel((mel4,light), [gcc,caffe], 2, []). %edge42

mel((mel5,full), [docker], 60, []). %cloud42
mel((mel5,medium), [gcc,caffe], 4, []). %edge42
mel((mel5,light), [docker], 20, []). %cloud42


mel2mel(usersData, videoStorage, 70).
mel2mel(videoStorage, movementProcessing, 30).
mel2mel(movementProcessing, arDriver, 20).
mel2mel(mel1, mel2, 999). % latenza massima supportata

application((arApp, full), [(usersData,full), (videoStorage,full), (movementProcessing,full), (arDriver,full)]).
application((arApp, adaptive), [(usersData,full), (videoStorage,_), (movementProcessing,_), (arDriver,_)]).
application((app1, adaptive), [(mel1,_), (mel2,_)]).
application((app2, adaptive), [(mel1,_), (mel2,_), (mel3,_), (mel4,_)]).
application((app3, adaptive), [(mel1,_), (mel2,_), (mel3,_), (mel4,_), (mel5,_)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%
% Example Infrastructure %
%%%%%%%%%%%%%%%%%%%%%%%%%%

node(edge42, [(gcc,0),(caffe,4)], (6, 3), [(phone,1),(lightSensor,1)]).
node(cloud42, [(docker, 5)], (100, 1), []).

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



