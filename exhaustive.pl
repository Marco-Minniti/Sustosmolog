:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- consult('core').
:- ['example'].
%:-['infra','app'].

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
    best(SortType,EvaluatedPlacements,BestPlacement).
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


