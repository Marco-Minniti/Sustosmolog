% set_prolog_flag(answer_write_options, [quoted(true), portray(true), max_depth(0), spacing(next_argument)]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- consult('core').
:- ['example'].
%:-['infra','app'].

/*

Singolo nodo:
% 0.7 carbone = 1.1 kgCO2-eq/kWh
% 0.1 petrolio = 1.0 kgCO2-eq/kWh
% 0.2 solare  = 0.02 kgCO2-eq/kWh

Format: node(N, Sw, Hw, Iot, ([(Fonte fossile, % usata)], [(Fonte rinnovabile, % usata)]) ). 
Es.
node(edge42, [(gcc,0),(caffe,4)], (6, 3), [(phone,1),(lightSensor,1)] ).
energyMix(edge42, [(Carbone, 0.7), (Petrolio, 0.1)], [(Solare, 0.2)]).
energyConsumption(NodeId, WattXHwUnit).
energyConsumption(edge42, 1).
node(cloud42, [(docker, 5)], (100, 1), [], ([(Petrolio, 0.6)], [(Solare, 0.4)]) ).

Format: co2(Fonte, kgCO2-eq/kWh).
co2(Carbone, 1.1).
co2(Petrolio, 1.0).
co2(Solare, 0.02).

% 0.5 kW consumo totale (delle risorse hw della soluzione(?)) della soluzione (insieme di placement)

% 0.5  (0.8 * 1.1 + 0.2 * 0.02 + 0.1 * 1.0)
Emissioni kgCO2-eq = (0.5 kW x 70% x 1.1 kgCO2-eq/kWh) + (0.5 kW x 10% x 1 kgCO2-eq/kWh) + (0.5 kW x 20% x 0.02 kgCO2-eq/kWh)

% Alternativa (+ semplice)
% - scegliere la soluzione che utilizza maggior quantità di energia rinnovabile (in altre parole, il placement che riesce quindi a soddifare la richiesta e a utilizzare maggiormente un nodo che utilizza una maggior % di energia rinnovabile (?)).
% - 20% di 0.5 kWh è rinnovabile
% - è migliore un placement dove è rinnovabile il 60% ...
*/



%-----------------------------------------------------------------------------------------------

% Request format: request(SortType, AppId, AppVersion, PreferredMELVersion, MaxCost, UserID). 
request((0,highest), app1, adaptive, full, 999).
% request((0,highest), app2, adaptive, full, 999).
% request((0,highest), app3, adaptive, full, 999).
request((0,highest), arApp, adaptive, full, 110).

%-----------------------------------------------------------------------------------------------

:- dynamic deploymentsInfos/4.
% deploymentsInfos(X, Y, Z, A).

/*
?- best(BestPlacements,BestProfit).
BestPlacements = [s([on(usersData, full, cloud42), on(videoStorage, medium, cloud42), on(movementProcessing, full, cloud42), on(arDriver, medium, edge42)], 107), s([on(mel1, medium, cloud42), on(mel2, light, edge42)], 24)],
BestProfit = 131.

sort([sol(2, [s([on(usersData, full, cloud42), on(videoStorage, full, cloud42), on(movementProcessing, full, cloud42), on(..., ..., ...)], 107), s([on(mel1, medium, cloud42), on(mel2, light, edge42)], 24)]),sol(3, [s([on(usersData, full, cloud42), on(videoStorage, full, cloud42), on(movementProcessing, full, cloud42), on(..., ..., ...)], 108), s([on(mel1, medium, cloud42), on(mel2, light, edge42)], 24)]), sol(1, [s([on(usersData, full, cloud42), on(videoStorage, full, cloud42), on(movementProcessing, full, cloud42), on(..., ..., ...)], 107), s([on(mel1, medium, cloud42), on(mel2, light, edge42)], 24)])], Sorted),
reverse(Sorted, Reversed).

*/

best(BestPlacements,BestProfit) :-
    % prende tutte le richieste da gestire
    findall((SortType, AppId, AppVersion, PreferredMELVersion, MaxCost), request(SortType, AppId, AppVersion, PreferredMELVersion, MaxCost), As),
    % trova tutti i piazzamenti possibili per tutte le applicazioni, con il relativo profitto totale
    findall(sol(Profit,Placements, HwUsed),  ( place([], As, Placements, [], HwUsed), profit(Placements, Profit) ), Solutions), % sol(131, [s([on(usersData, full, cloud42), on(videoStorage, full, cloud42), on(movementProcessing, full, cloud42), on(..., ..., ...)], 107), s([on(mel1, medium, cloud42), on(mel2, light, edge42)], 24)], [(cloud42, 97), (edge42, 3)])
    % ordina le soluzioni in ordine decrescente di profitto e prendi la (prima) soluzione ottima
    sort(Solutions, Tmp), reverse(Tmp, Reversed), % [sol(BestProfit,BestPlacements, _)|_] % prende il primo elemento, il resto non mi interessa
    updateDeploymentState(Reversed, BestPlacements, BestProfit). 
    %prendere il primo, però vedere cosa succede se fallisce la reverse, perchè secondo me va eliminata, devo far in modo di reversare tutta la lista, e non prendere solo il primo elemento ma scorrerla, e fare in modo che se i controlli falliscono va all'elemento dopo 


% Query : updateDeploymentState([sol(999, [s([on(usersData, full, cloud42), on(videoStorage, full, cloud42), on(movementProcessing, full, cloud42)], 107), s([on(mel1, medium, cloud42), on(mel2, light, edge42)], 24)], [(cloud42, 99999), (edge42, 3)]), sol(131, [s([on(usersData, full, cloud42), on(videoStorage, full, cloud42), on(movementProcessing, full, cloud42)], 107), s([on(mel1, medium, cloud42), on(mel2, light, edge42)], 24)], [(cloud42, 97), (edge42, 3)]), sol(1, [s([on(usersData, full, cloud42), on(videoStorage, full, cloud42), on(movementProcessing, full, cloud42)], 107), s([on(mel1, medium, cloud42), on(mel2, light, edge42)], 24)], [(cloud42, 97), (edge42, 3)])], BestPlacements, BestProfit).
% Out: BestPlacements = [s([on(usersData, full, cloud42), on(videoStorage, full, cloud42), on(movementProcessing, full, cloud42)], 107), s([on(mel1, medium, cloud42), on(mel2, light, edge42)], 24)],
%      BestProfit = 131.
% Easy query: updateDeploymentState([sol(999, [1], [(cloud42, 99999), (edge42, 3)]), sol(131, [2], [(cloud42, 97), (edge42, 3)]), sol(1, [3], [(cloud42, 97), (edge42, 3)])], BestPlacements, BestProfit).
updateDeploymentState(Reversed, BestPlacements, BestProfit) :- updateDeploymentState(Reversed, [], 0, BestPlacements, BestProfit).

updateDeploymentState([], BestPlacements, BestProfit, BestPlacements, BestProfit).
updateDeploymentState([sol(Profit,Placements,AllocatedHW)|_], _, _, BestPlacements, BestProfit) :-
    greenCheck(AllocatedHW, CO2, E),
    % greenCheck(AllocatedHW, SustParam),
    updateDeploymentsInfos(sol(Profit,Placements,AllocatedHW), [(co2, CO2), (energy, E)]), % SOSTITUIRE CON VARIABILE LISTA TIPO SustParam = [(sustParamName, value)].
    updateDeploymentState([], Placements, Profit, BestPlacements, BestProfit), !.
updateDeploymentState([sol(_,_,_)|Rest], TmpPlacements, TmpProfit, BestPlacements, BestProfit) :-
    updateDeploymentState(Rest, TmpPlacements, TmpProfit, BestPlacements, BestProfit).


% Query: updateDeploymentsInfos(sol(131, [s([on(usersData, full, cloud42), on(videoStorage, full, cloud42), on(movementProcessing, full, cloud42)], 107), s([on(mel1, medium, cloud42), on(mel2, light, edge42)], 24)], [(cloud42, 97), (edge42, 3)]), [(co2, 295.374), (energy, 203)]).
% Out: deploymentsInfos(X, Y, Z, A).
updateDeploymentsInfos(sol(Profit,Placements,AllocatedHW), SustParam) :- 
    deploymentsInfos(OldProfit, OldPlacements, OldAllocatedHW, OldSustParam), 
    NewProfit is OldProfit + Profit,
    merge(AllocatedHW, OldAllocatedHW, NewHw),
    merge(SustParam, OldSustParam, NewSustParam),
    append(Placements, OldPlacements, NewPlacements),
    retract(deploymentsInfos(OldProfit, OldPlacements, OldAllocatedHW, OldSustParam)), assert(deploymentsInfos(NewProfit, NewPlacements, NewHw, NewSustParam)).
updateDeploymentsInfos(sol(Profit,Placements,AllocatedHW), SustParam) :- 
    \+ deploymentsInfos(_,_,_,_), assert(deploymentsInfos(Profit,Placements,AllocatedHW, SustParam)).


 greenCheck(AllocatedHW, CO2, E) :-
    footprint(AllocatedHW, 0, E, 0, CO2),
    targetsOk(E, CO2).

% calcola il profitto totale dei piazzamenti
profit([s(_,ProfitP)|Ps], Profit) :-
    profit(Ps,TmpProfit),
    Profit is ProfitP + TmpProfit.
profit([], 0).

% OldHw format = AllocatedHW format: [(Node, HwUsed)].
place(Placements, [A|As], NewPlacements, OldHw, HwUsed) :-
    % piazza l'applicazione A in uno dei modi possibili tenendo conto dell'hardware utilizzato dai piazzamenti precedentemente effettuati presenti in "Placements"
    place_app(Placements, A, TempPlacements, OldHw, NewHw),
    place(TempPlacements, As, NewPlacements, NewHw, HwUsed). % ricorre sulle altre applicazioni
place(Placements, [_|As], NewPlacements, OldHw, HwUsed) :-
    % non piazzare l'applicazione A, ricorre quindi sulle altre applicazioni
    place(Placements, As, NewPlacements, OldHw, HwUsed). 
place(Placements, [], Placements, OldHw, OldHw). % caso base: non ci sono altre applicazioni da piazzare

place_app(Placements, (_, AppId, AppVersion, _, MaxCost), [s(Placement, PlacementProfit)|Placements], OldHw, NewHw) :-
    % ottiene un singolo piazzamento ammissibile di A, tenendo conto delle risorse Hw già allocate, presenti in OldHw
    % restuiendomi così il Placement che concatena insieme agli altri per la creazione della lista soluzione, il profitto e l'hw allocato del placement 
    placement(AppId, AppVersion, MaxCost, Placement, PlacementProfit, AllocatedHW, OldHw),
    % footprint(AllocatedHW, 0, E, 0, CO2),
    % targetsOk(E, CO2),
    % aggiorna le risorse Hw allocate
    merge(AllocatedHW, OldHw, NewHw).

/*
% GENERALIZZATA
% targetsOk([E,Co2], OutList).
% targetsOk([100,200], [], OutList). 
% Out: OutList = [(energy, 100), (co2, 200)]
targetsOk([], Acc, Acc).
targetsOk([Value|Rest], Acc, SustParam) :- % per generalizzare, considero che ogni elemento passato nella lista corrisponde all'elemento definito nella rispettiva posizione (es. Primo elemento: E di energia -> devo far in modo che prolog quando prende target(Param, Value), prende energia).
    target(Param, Max), % qui problema backtracking, se lo blocco "!" riparte sempre da "energy" ad ogni call, se non lo blocco, il bcktrk riparte da qui dando in out 4 risultati.
    ( deploymentsInfos(_, _, _, OldSustParam), member((Param, ValueAsserted), OldSustParam); \+ deploymentsInfos(_, _, _, _), ValueAsserted is 0 ),
    Value + ValueAsserted =< Max,
    targetsOk(Rest, [(Param, Value)|Acc], SustParam).   
*/

targetsOk(E, CO2) :-
    ( deploymentsInfos(_, _, _, OldSustParam), member((co2, Co2Asserted), OldSustParam), member((energy, EnergyAsserted), OldSustParam); \+ deploymentsInfos(_, _, _, _), Co2Asserted is 0, EnergyAsserted is 0 ),
    target(co2, CMax), CO2 + Co2Asserted =< CMax,
    target(energy, EMax), E + EnergyAsserted =< EMax.


carbonIntensity(N, C) :- 
    energyMix(N, Sources),
    avgCI(Sources, C).

avgCI(S, C) :- avgCI(S, 0, C).

avgCI([(S, P)|Ss], OldC, NewC) :-
    co2(S, Cs),
    TmpC is OldC + P * Cs,
    avgCI(Ss, TmpC, NewC).
avgCI([], C, C).

% footprint( [(edge42, 2), (cloud42, 80)], 0, NewW, 0, NewCO2).
footprint([(N,H)|As], OldW, NewW, OldCO2, NewCO2) :-
    energyConsumption(N, EnergyXHw), TmpW is OldW + (H * EnergyXHw),
    carbonIntensity(N, C), TmpCO2 is OldCO2 + C * TmpW, 
    footprint(As, TmpW, NewW, TmpCO2, NewCO2).
footprint([], W, W, C, C).

merge([(N,H)|As], OldHw, NewHw) :-
    member((N,H1), OldHw),
    NewH is H1+H, select((N,H1), OldHw, (N,NewH), TmpHw),
    merge(As,TmpHw, NewHw).
merge([(N,H)|As], OldHw, NewHw) :-
    \+ member((N,H), OldHw),
    merge(As, [(N,H)|OldHw], NewHw).
merge([],H,H).

%-----------------------------------------------------------------------------------------------

go(SortType, AppName, AppVersion, PreferredMelVersion, MaxCost, VC, C, Best, Time) :-
statistics(cputime, Start),
goForBest(SortType, AppName, AppVersion, PreferredMelVersion, MaxCost, BestPlacement),
statistics(cputime, Stop), Time is Stop - Start,
nth0(1,BestPlacement,VC), nth0(2,BestPlacement,C), nth0(3,BestPlacement,Best).


% goForBest((0,highest), arApp, adaptive, full, 110, BestPlacement, PlacementsHw, []).
% goForBest((0,highest), testApp, adaptive, full, 999, BestPlacement, PlacementsHw, []).
goForBest(SortType, AppName, AppVersion, PreferredMelVersion, MaxCost, BestPlacement, PlacementsHw, OldHw) :-
    findall((Placement, PlacementProfit, AllocatedHW), placement(AppName, AppVersion, MaxCost, Placement, PlacementProfit, AllocatedHW, OldHw), PlacementsHw),
    maplist(extractPlacements, PlacementsHw, Placements),
    evalPlacements(AppName, AppVersion, PreferredMelVersion, Placements, EvaluatedPlacements),
    best(SortType,EvaluatedPlacements,BestPlacement).

extractPlacements((Placement, PlacementProfit, _), (Placement, PlacementProfit)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% placement(arApp, adaptive, 110, Placement, PlacementProfit, AllocatedHW, []).
% placement(testApp, adaptive, 999, Placement, PlacementProfit, AllocatedHW, []).
placement(AppName, AppVersion, MaxCost, Placement, TotCost, AllocatedHW, OldHw) :-
    application((AppName, AppVersion), MELs),
    melPlacementOK(MELs, Placement, [], 0, TotCost, MaxCost, AllocatedHW, OldHw),
    findall(mel2mel(M1,M2,Latency), mel2mel_in_placement(M1,M2,Latency,Placement), FlowConstraints),
    flowsOK(FlowConstraints, Placement).

% evalPacements ranks a list of placements
evalPlacements(_, _, _, [], []).
evalPlacements(AppName, AppVersion, PreferredMelVersion, [(Placement,Cost)], [[_, VersionCompliance, Cost, Placement]]):-
    application((AppName, AppVersion), Mels), length(Mels, NMels), 
    findall(S, member(on(S, PreferredMelVersion, _), Placement), Ls), length(Ls, NPreferredVersionMels),
    VersionCompliance is div(100*NPreferredVersionMels,NMels).
    evalPlacements(AppName, AppVersion, PreferredMelVersion, Placements, EvaluatedPlacements):-
    length(Placements, L), L>1, 
    application((AppName, AppVersion), Mels), length(Mels, NMels),
    maxANDmin(Placements, MinAllCosts, MaxAllCosts),
    findall([Formula, VersionCompliance, Cost, Placement], 
            (member((Placement, Cost), Placements), 
            findall(S, member(on(S, PreferredMelVersion, _), Placement), Ls), length(Ls, NPreferredVersionMels),
            VersionCompliance is div(100*NPreferredVersionMels,NMels),
            ( (MaxAllCosts-MinAllCosts > 0, NormalizedCost is div((100*(MaxAllCosts - Cost)),(MaxAllCosts - MinAllCosts))); NormalizedCost is 100 ),
            Formula is VersionCompliance + NormalizedCost),
            EvaluatedPlacements).

maxANDmin([(_, Cost)|Rest], MinCost, MaxCost) :- 
    length(Rest,L),L>0,
    maxANDmin(Rest, RestMinCost, RestMaxCost),
    ((Cost =< RestMinCost, MinCost is Cost); (Cost > RestMinCost, MinCost is RestMinCost)),
    ((Cost >= RestMaxCost, MaxCost is Cost); (Cost < RestMaxCost, MaxCost is RestMaxCost)).
    maxANDmin([(_, Cost)], Cost, Cost). 

best(_, [], none).
best(_, [P], P).
best(ST, EPs, BestP) :- length(EPs, L), L>1, best2(ST, EPs, BestP).
best2(_, [E], E).
best2(ST, [E|Es], BestP) :- length(Es, L), L>0, best2(ST, Es, BestOfEs), choose(ST, E, BestOfEs, BestP).
choose((S,highest), E, BestOfEs, E) :- nth0(S, E, V), nth0(S, BestOfEs, W), V > W.
choose((S,highest), E, BestOfEs, BestOfEs) :- nth0(S, E, V), nth0(S, BestOfEs, W), V =<  W.
choose((S,lowest), E, BestOfEs, E) :- nth0(S, E, V), nth0(S, BestOfEs, W), V =< W.
choose((S,lowest), E, BestOfEs, BestOfEs) :- nth0(S, E, V), nth0(S, BestOfEs, W), V > W.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Returns the best and worst placement (BestP, WorstP) out of all placements, with statistics on CPU time.
% It also returns the best and worst ranking, version compliance and cost.
% Given an input placement HeuP, it evaluates the same also for it.
% Sample query:
% goForAll(arApp, adaptive, full, 110, HeuP, HeuF, BestP, BestF, BestVC, BestC, WorstP, WorstF, WorstVC, WorstC, Time).
goForAll(AppName, AppVersion, PreferredMelVersion, MaxCost, HeuP, HeuF, BestP, BestF, BestVC, BestC, WorstP, WorstF, WorstVC, WorstC, Time) :-
    statistics(cputime, Start),
findall((Placement, PlacementProfit), placement(AppName, AppVersion, MaxCost, Placement, PlacementProfit), Placements),
evalPlacements(AppName, AppVersion, PreferredMelVersion, Placements, EvaluatedPlacements),
sort(1,@>=,EvaluatedPlacements, SPlacements),
SPlacements=[Best|_],
    statistics(cputime, Stop),
    Time is Stop - Start,
nth0(0,Best,BestF),nth0(1,Best,BestVC), nth0(2,Best,BestC), nth0(3,Best,BestP),
last(SPlacements, Worst),
nth0(0,Worst,WorstF),nth0(1,Worst,WorstVC), nth0(2,Worst,WorstC), nth0(3,Worst,WorstP),
findHeuristicF(HeuP, HeuF, SPlacements).

findHeuristicF(HeuP, HeuF, SPlacements):-
member([PF,_,_, P], SPlacements),
sort(HeuP,HeuPSorted), sort(P,PSorted),
PSorted = HeuPSorted,
( (ground(PF), HeuF is PF) ; HeuF = 200).

myPrint([]).
myPrint([X|Xs]):-write_ln(X),myPrint(Xs).
