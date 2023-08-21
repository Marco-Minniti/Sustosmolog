:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- consult('core').
:- ['example'].
%:-['infra','app'].


:- dynamic deployment/3.
:- dynamic usedHw/2.


%-----------------------------------------------------------------------------------------------
%USER FORMAT: user(id, plan)
user(u1, premium).
user(u2, trial).
user(u3, trial).

%REQUEST FORMAT: request(SortType, AppId, AppVersion, PreferredMELVersion, MaxCost, UserID). 
request((0,highest), arApp, adaptive, full, 110). %query
request((0,highest), arApp, adaptive, full, 107).
request((0,highest), arApp, adaptive, full, 107).
%-----------------------------------------------------------------------------------------------

%-----------------------------------------------------------------------------------------------
placementRequests(Result) :- %query: placementRequests(Requests). -->Requests = [request((0,highest),arApp,adaptive,full,110,1), request((0,highest),arApp,adaptive,full,110,2), request((0,highest),arApp,adaptive,full,99,1) ]
findall((SortType, AppId, AppVersion, PreferredMELVersion, MaxCost), request(SortType, AppId, AppVersion, PreferredMELVersion, MaxCost), Requests),
processRequests(Requests, [], Result).


processRequests([], Acc, Acc).
processRequests([(SortType, AppId, AppVersion, PreferredMELVersion, MaxCost) | Rest], Acc, Result) :-
goForBest(SortType, AppId, AppVersion, PreferredMELVersion, MaxCost, BestPlacement),
NewAcc = [BestPlacement | Acc],
processRequests(Rest, NewAcc, Result).

%findall(goForBest(SortType, AppId, AppVersion, PreferredMELVersion, MaxCost, UserID), request(SortType, AppId, AppVersion, PreferredMELVersion, MaxCost, UserID), Requests),
%per dare priorità a quelli premium lo potrei fare mettendo nella clausola centrale che ti cerchi solo gli user(_, premium) 
%process_requests(Requests).

%placementRequest(Request, BestPlacement) %predicato che elabora le richieste
%-----------------------------------------------------------------------------------------------



% Finds the best solution, relying on goForBest and returns CPU time.
% Example query: go((0,highest), arApp, adaptive, full, 100, V, C, Best, Time).
%%
go(SortType, AppName, AppVersion, PreferredMelVersion, MaxCost, VC, C, Best, Time) :-
    statistics(cputime, Start),
    goForBest(SortType, AppName, AppVersion, PreferredMelVersion, MaxCost, BestPlacement),
    statistics(cputime, Stop), Time is Stop - Start,
    nth0(1,BestPlacement,VC), nth0(2,BestPlacement,C), nth0(3,BestPlacement,Best).

% goForBest returns the "best" full ranked placement according to the given SortType
%   SortType is a couple (S,highest/lowest) to indicate:
%   - the ranking value with respect to which sort results, i.e. ranking (S=0), version compliance (S=1), cost (S=2), and
%   - how to sort (Highest or lowest)
% Sample queries:
%   goForBest((0,highest), smartHome, dunno, full, 40, Best).
%   goForBest(prolog, (2,lowest),  smartHome, dunno, full, 40, Best).
%   goForBest(clp,    (1,lowest),  smartHome, dunno, full, 40, Best).
goForBest(SortType, AppName, AppVersion, PreferredMelVersion, MaxCost, BestPlacement) :-
    findall((Placement, PlacementCost), placement(AppName, AppVersion, MaxCost, Placement, PlacementCost), Placements),
    evalPlacements(AppName, AppVersion, PreferredMelVersion, Placements, EvaluatedPlacements),
    best(SortType,EvaluatedPlacements,BestPlacement),
    
    hwUsed(BestPlacement, HwUsed), % [all(Node,AllocatedHw),...] % Considero fattibile il placement arrivato fin qui, da questo estraggo i nomi dei servizi, calcolo l'hw COMPLESSIVO, aggiorno la kb.
    assert(deployment(AppName, BestPlacement, HwUsed)). % --> deployment(fooApp, [on(s1,small,n1), on(s2, large, n24), on(s1, small, n1)], [ all(n1,10), all(n24,8) ]).
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    placement(AppName, AppVersion, MaxCost, Placement, TotCost) :-
    application((AppName, AppVersion), MELs),
    melPlacementOK(MELs, Placement, [], 0, TotCost, MaxCost),
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
findall((Placement, PlacementCost), placement(AppName, AppVersion, MaxCost, Placement, PlacementCost), Placements),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Query: hwUsed([175, 75, 107, [on(usersData, full, cloud42), on(videoStorage, full, cloud42), on(movementProcessing, full, cloud42), on(arDriver, light, edge42)]], HwUsed).
% Out: HwUsed = [all(edge42, 1), all(cloud42, 88)] .
hwUsed(P, HwUsed) :-
    extractSublist(P, _, Sublist), 
    getNodes(Sublist, [], Nodes),   
    placementHwUsed(Nodes, Sublist, HwUsed), 
    assertUsedHw(HwUsed).

% Query: assertUsedHw([all(edge42, 1), all(cloud42, 88)]).
assertUsedHw([]).
assertUsedHw([all(N,UsedHwAtN)|Rest]) :- 
    \+ usedHw(N,_),
    assertUsedHw(Rest),
    assert(usedHw(N,UsedHwAtN)).
assertUsedHw([all(N,UsedHwAtN)|Rest]) :- 
    usedHw(N,OldHw), 
    assertUsedHw(Rest),
    NewHw is OldHw + UsedHwAtN, 
    retract(usedHw(N,OldHw)), assert(usedHw(N,NewHw)).


% Query: placementHwUsed([edge42, cloud42], [on(usersData, full, cloud42), on(videoStorage, full, cloud42), on(movementProcessing, full, cloud42), on(arDriver, light, edge42)], HwUsed).
% Out: HwUsed = [all(edge42, 1), all(cloud42, 88)].
placementHwUsed([],_,[]).         
placementHwUsed([N|Rest], Sublist, [all(N,UsedHwAtN)|HwUsed]) :- 
    placementHwUsed(Rest, Sublist, HwUsed),
    findall(HwReqs, (member(on(S,V,N), Sublist), mel((S,V),_,HwReqs,_)), SingleUsedHWs), % Voglio trovare tutti gli elementi on(S, V, N) in Sublist, preso un elemento es on(usersData, full, cloud42), mi ricavo il relativo mel((S,V),_,HwReqs,_) ovvero mel((usersData,full),_,64,_), da cui estraggo HwReqs=64 e lo metto in una lista SingleUsedHWs
    sumlist(SingleUsedHWs, UsedHwAtN). 

% Query: extractSublist([175, 75, 107, [on(usersData, full, cloud42), on(videoStorage, full, cloud42), on(movementProcessing, full, cloud42), on(arDriver, light, edge42)]], _, Sublists).
% Out: Sublists = [on(usersData, full, cloud42), on(videoStorage, full, cloud42), on(movementProcessing, full, cloud42), on(arDriver, light, edge42)]
extractSublist([], Sublists, Sublists).
extractSublist([Item|Rest], _, Sublist) :-
    extractSublist(Rest, Item, Sublist).

% Query: getNodes([on(usersData, full, cloud42), on(videoStorage, full, cloud42), on(movementProcessing, full, cloud42), on(arDriver, light, edge42)], [], Nodes).
% Out: Nodes = [edge42, cloud42]
getNodes([], Nodes, Nodes).
getNodes([on(_,_,N)|Rest], Acc, Nodes) :-
    member(N, Acc),
    getNodes(Rest, Acc, Nodes).
getNodes([on(_,_,N)|Rest], Acc, Nodes) :-
    \+ member(N, Acc),
    NewList = [N | Acc],
    getNodes(Rest, NewList, Nodes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%