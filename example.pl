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

mel2mel(usersData, videoStorage, 70).
mel2mel(videoStorage, movementProcessing, 30).
mel2mel(movementProcessing, arDriver, 20).

application((arApp, full), [(usersData,full), (videoStorage,full), (movementProcessing,full), (arDriver,full)]).
application((arApp, adaptive), [(usersData,full), (videoStorage,_), (movementProcessing,_), (arDriver,_)]).

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