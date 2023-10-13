% set_prolog_flag(answer_write_options, [quoted(true), portray(true), max_depth(0), spacing(next_argument)]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- consult('core').
:- ['example'].
%:-['infra','app'].

/*
TODO:
• Aggiunta, rimozione e modifica dei requisiti dei mel.
• Cambiamento/Crash di un nodo, cambiamento/Crash di un link
• Script python che simula CR su input variabili e raccoglie tempi esecuzione, emissioni e profitto
NEW:
• Gestire flavours sorting.
*/

%-----------------------------------------------------------------------------------------------

% Request format: request(SortType, AppId, AppVersion, PreferredMELVersion, MaxCost, UserID). 
 % request((0,highest), testApp, adaptive, full, 999).
 request((0,highest), app1, adaptive, full, 999).
 request((0,highest), arApp, adaptive, full, 110).
 %request((0,highest), appCR, adaptive, full, 999).
 

%-----------------------------------------------------------------------------------------------

:- dynamic deploymentsInfos/4.

% deploymentsInfos(X, Y, Z, A).

:- dynamic deploymentsDetails/1. 
% deploymentsDetails(X).

best(BestPlacements,BestProfit) :-
    findall((SortType, AppId, AppVersion, PreferredMELVersion, MaxCost), request(SortType, AppId, AppVersion, PreferredMELVersion, MaxCost), As),
    (deploymentsDetails(OldHw); \+ deploymentsDetails(_), OldHw = []),
    filterDeploymentsDetails(OldHw, FilteredHw),
    findall(sol(Profit,Placements, HwUsed),  ( place([], As, Placements, FilteredHw, HwUsed), profit(Placements, Profit) ), Solutions), 
    byDecreasingProfit(Solutions, SortedSols),
    updateDeploymentState(SortedSols, BestPlacements, BestProfit).

byDecreasingProfit(Sols, SortedSols) :- sort(Sols, Tmp), reverse(Tmp, SortedSols).

filterDeploymentsDetails([],[]).
filterDeploymentsDetails([(AppId, Hw, _, _)|Rest], [(AppId,Hw)|FilteredHw]) :-
    filterDeploymentsDetails(Rest, FilteredHw).

updateDeploymentState(Reversed, BestPlacements, BestProfit) :- updateDeploymentState(Reversed, [], 0, BestPlacements, BestProfit).
updateDeploymentState([], BestPlacements, BestProfit, BestPlacements, BestProfit).
updateDeploymentState([sol(Profit,Placements,AllocatedHW)|_], _, _, BestPlacements, BestProfit) :-
    greenCheck(AllocatedHW, [], SustParam), % SustParam = [(app1, [(co2, 19.836), (energy, 24)]), (arApp, [(co2, 106.172), (energy, 166)])].
    updateDeploymentsInfos(sol(Profit,Placements,AllocatedHW), SustParam, CutPlacements),
    updateDeploymentState([], CutPlacements, Profit, BestPlacements, BestProfit), !.
updateDeploymentState([sol(_,_,_)|Rest], TmpPlacements, TmpProfit, BestPlacements, BestProfit) :-
    updateDeploymentState(Rest, TmpPlacements, TmpProfit, BestPlacements, BestProfit).


updateDeploymentsInfos(sol(_,Placements,_), SustParam, OutCutPlacements) :- 
    deploymentsInfos(OldProfit, OldPlacements, OldAllocatedHW, OldSustParam),
    deploymentsDetails(OldDetails),
    updateDeploymentsDetails(Placements, SustParam, [], OldProfit, OldPlacements, OldAllocatedHW, OldSustParam, OldDetails, OutCutPlacements, OutTotalPr, OutPlacements, OutTotalHw, OutTotalSus, NewDetails),
    retract(deploymentsInfos(OldProfit, OldPlacements, OldAllocatedHW, OldSustParam)), assert(deploymentsInfos(OutTotalPr,OutPlacements,OutTotalHw, OutTotalSus)),
    retract(deploymentsDetails(OldDetails)), assert(deploymentsDetails(NewDetails)).

updateDeploymentsInfos(sol(_,Placements,_), SustParam, OutCutPlacements) :- 
    \+ deploymentsInfos(_,_,_,_), 
    updateDeploymentsDetails(Placements, SustParam, [], 0, [], [], [], [], OutCutPlacements, OutTotalPr, OutPlacements, OutTotalHw, OutTotalSus, NewDetails),
    assert(deploymentsInfos(OutTotalPr,OutPlacements,OutTotalHw, OutTotalSus)),
    assert(deploymentsDetails(NewDetails)).


% updateDeploymentsDetails([s(appCR, [on(s1, full, n2), on(s2, full, n2)], 6, [(appCR, [(n2, 7)])])], [(appCR, [(co2, 2), (energy, 3)])], [], 10, [s(appCR, [on(s1, full, n1), on(s2, full, n2)])], [(n1, 4), (n2, 6)], [(energy, 0), (co2, 0)], [(appCR, [(n2, 6), (n1, 4)], 10, [(co2, 0), (energy, 0)])], OutCutPlacements, OutTotalPr, OutPlacements, OutTotalHw, OutTotalSus, NewDetails).
updateDeploymentsDetails([], _, CutPlacements, AssertTotalPr, AssertPlacements, AssertTotalHw, AssertTotalSus, NewDetails, CutPlacements, AssertTotalPr, AssertPlacements, AssertTotalHw, AssertTotalSus, NewDetails).
updateDeploymentsDetails([s(AppId, NewMels, NewProfit, NewAllocatedHw) | Rest], SustParam, CutPlacements, TmpOldProfit, TmpOldPlacements, TmpOldAllocatedHW, TmpOldSustParam, TmpOldDetails, OutCutPlacements, OutTotalPr, OutPlacements, OutTotalHw, OutTotalSus, OutDetails) :-
    % ----- deploymentsDetails -----
    member((AppId, OHw, OPr, OSus), TmpOldDetails), % REPLACE
    member((AppId, NewHw), NewAllocatedHw),  
    member((AppId, NewSus), SustParam),  
    select((AppId, _, _, _), TmpOldDetails, (AppId, NewHw, NewProfit, NewSus), NewDetails), 
    % ----- deploymentsInfos -----
    select((s(AppId, _)), TmpOldPlacements, (s(AppId, NewMels)), AssertPlacements),
    updateTotalAllocatedHw(NewHw, OHw, TmpOldAllocatedHW, [], AssertTotalHw), 
    updateTotalSustainability(NewSus, OSus, TmpOldSustParam, AssertTotalSus),
    AssertTotalPr is TmpOldProfit-OPr+NewProfit,
    updateDeploymentsDetails(Rest, SustParam, [s(AppId, NewMels)|CutPlacements], AssertTotalPr, AssertPlacements, AssertTotalHw, AssertTotalSus, NewDetails, OutCutPlacements, OutTotalPr, OutPlacements, OutTotalHw, OutTotalSus, OutDetails).

updateDeploymentsDetails([s(AppId, NewMels, NewProfit, NewAllocatedHw) | Rest], SustParam, CutPlacements, TmpOldProfit, TmpOldPlacements, TmpOldAllocatedHW, TmpOldSustParam, TmpOldDetails, OutCutPlacements, OutTotalPr, OutPlacements, OutTotalHw, OutTotalSus, OutDetails) :-
    % ----- deploymentsDetails -----
    \+ member((AppId,_,_,_) , TmpOldDetails),
    member((AppId, NewHw), NewAllocatedHw),
    member((AppId, NewSus), SustParam),  
    append(TmpOldDetails, [(AppId, NewHw, NewProfit, NewSus)], NewDetails),
    % ----- deploymentsInfos -----
    append([s(AppId, NewMels)], TmpOldPlacements,  AssertPlacements),
    merge(NewHw, TmpOldAllocatedHW, AssertTotalHw),
    merge(NewSus, TmpOldSustParam, AssertTotalSus),
    AssertTotalPr is TmpOldProfit + NewProfit,
    updateDeploymentsDetails(Rest, SustParam, [s(AppId, NewMels)|CutPlacements], AssertTotalPr, AssertPlacements, AssertTotalHw, AssertTotalSus, NewDetails, OutCutPlacements, OutTotalPr, OutPlacements, OutTotalHw, OutTotalSus, OutDetails).

updateDeploymentsDetails(Placements, SustParam, CutPlacements, TmpOldProfit, TmpOldPlacements, TmpOldAllocatedHW, TmpOldSustParam, TmpOldDetails, OutCutPlacements, OutTotalPr, OutPlacements, OutTotalHw, OutTotalSus, OutDetails) :-
    \+ deploymentsDetails(_), assert(deploymentsDetails([])), updateDeploymentsDetails(Placements, SustParam, CutPlacements, TmpOldProfit, TmpOldPlacements, TmpOldAllocatedHW, TmpOldSustParam, TmpOldDetails, OutCutPlacements, OutTotalPr, OutPlacements, OutTotalHw, OutTotalSus, OutDetails).

updateTotalSustainability(NewSus, OSus, OldSustParam, AssertTotalSus) :- updateTotalAllocatedHw(NewSus, OSus, OldSustParam, [], AssertTotalSus).

% updateTotalAllocatedHw([(n2, 7)], [(n2, 6), (n1, 4)], [(n1, 4), (n2, 6)], [], Out).
updateTotalAllocatedHw([],_,_,Out,Out).
updateTotalAllocatedHw([(N,H)|Rest], OldHw, OldAllocatedHW, AssertTotalHw, Out) :-
    member((N,H1), OldHw), member((N,H2), OldAllocatedHW),
    NewH is H2-H1+H,
    updateTotalAllocatedHw(Rest, OldHw, OldAllocatedHW, [(N, NewH)|AssertTotalHw], Out), !.
updateTotalAllocatedHw([(N,H)|Rest], OldHw, OldAllocatedHW, AssertTotalHw, Out) :-
    \+ member((N,_), OldAllocatedHW),
    updateTotalAllocatedHw(Rest, OldHw, OldAllocatedHW, [(N, H)|AssertTotalHw], Out).
updateTotalAllocatedHw([(N,H)|Rest], OldHw, OldAllocatedHW, AssertTotalHw, Out) :-
    member((N,H2), OldAllocatedHW),
    NewH is H2+H,
    updateTotalAllocatedHw(Rest, OldHw, OldAllocatedHW, [(N, NewH)|AssertTotalHw], Out).

% greenCheck([(arApp, [(edge42, 2), (cloud42, 80)]), (app1, [(edge42, 2), (cloud42, 9)])], [], SustParam).
greenCheck([], TmpList, TmpList).
greenCheck([(AppId, HwUsed)|Rest], TmpList, SustParam) :-
    footprint(HwUsed, 0, E, 0, CO2),
    targetsOk(E, CO2),
    greenCheck(Rest, [(AppId, [(co2, CO2), (energy, E)])|TmpList], SustParam).


% calcola il profitto totale dei piazzamenti
profit([s(_,_,ProfitP,_)|Ps], Profit) :-
    profit(Ps,TmpProfit),
    Profit is ProfitP + TmpProfit.
profit([], 0).

% place_app([], ((0, highest), appCR, adaptive, full, 999), TempPlacements, [], NewHw).
% place([], [((0, highest), app1, adaptive, full, 999), ((0, highest), arApp, adaptive, full, 110)], TempPlacements, [], HwUsed).
% place([], [((0, highest), arApp, adaptive, full, 110)], TempPlacements, [], HwUsed).
place(Placements, [A|As], NewPlacements, OldHw, HwUsed) :-
    % piazza l'applicazione A in uno dei modi possibili tenendo conto dell'hardware utilizzato dai piazzamenti precedentemente effettuati presenti in "Placements"
    place_app(Placements, A, TempPlacements, OldHw, NewHw),
    place(TempPlacements, As, NewPlacements, NewHw, HwUsed). % ricorre sulle altre applicazioni
place(Placements, [_|As], NewPlacements, OldHw, HwUsed) :-
    % non piazzare l'applicazione A, ricorre quindi sulle altre applicazioni
    place(Placements, As, NewPlacements, OldHw, HwUsed). 
place(Placements, [], Placements, OldHw, OldHw). % caso base: non ci sono altre applicazioni da piazzare


place_app(Placements, (_, AppId, AppVersion, _, MaxCost), [s(AppId, Placement, PlacementProfit, AllocatedHW)|Placements], OldHw, AllocatedHW) :-
    ((deploymentsInfos(_, Ps, _, _), \+  member(s(AppId,_), Ps)); \+ deploymentsInfos(_, _, _, _)),
    placement(AppId, AppVersion, MaxCost, Placement, PlacementProfit, OldHw, AllocatedHW).
place_app(Placements, (_, AppId, AppVersion, _, MaxCost), [s(AppId, Placement, PlacementProfit, AllocatedHW)|Placements], OldHw, AllocatedHW) :-
    deploymentsInfos(_, Ps, _, _), member(s(AppId,P), Ps), % REPLACE APP
    newServices(AppId, AppVersion, P, NewServices), % newServices(appCR, adaptive, [on(s1, full, n1)], NewServices).
    reasoningStep(P, AppId, OldHw, NotOkServices, [], OkPlacement, [], OkHws), % reasoningStep([on(s1, full, n1), on(s2, full, n2)], [(n1, 1), (n2, 2)], NotOkServices, [], OkPlacement).
    append(NewServices,NotOkServices,ServicesToPlace),
    length(ServicesToPlace, Lenght), Lenght > 0,
    select((AppId,_), OldHw, (AppId,OkHws), UpdHw), % faccio in modo che gli arrivi un OldHw, INIZIALIZZATO, contenente solo i servizi OK
    placementCR(ServicesToPlace, AppId, MaxCost, PlacementProfit, UpdHw, AllocatedHW, OkPlacement, Placement). % placementCR([(s1,_)],999,Placement,TotProfit,AllocatedHw,[], [(n2, 1)], [on(s2, full, n2)]).

    
%  reasoningStep([on(s1, full, n1), on(s2, full, n2)], appCR, (appCR, [(n2, 6), (n1, 4)]), NotOkServices, [], OkPlacement, [], OkHws).
reasoningStep([on(S,V,_)|Ps], AppId, AllocHW, KOs, POk, StableP, HwOk, AllocatedHw) :-
    \+ mel((S,V),_,_,_),
    reasoningStep(Ps, AppId, AllocHW, KOs, POk, StableP, HwOk, AllocatedHw).
reasoningStep([on(S,V,N)|Ps], AppId, AllocHW, KOs, POk, StableP, HwOk, AllocatedHw) :- 
    placementOK(AppId, (S, V), N, _, AllocHW, _), !,
    mel((S,V),_,Hw,_), 
    reasoningStep(Ps, AppId, AllocHW, KOs, [on(S,V,N)|POk], StableP, [(N, Hw)|HwOk], AllocatedHw).
reasoningStep([on(S,_,_)|Ps], AppId, AllocHW,[(S,_)|KOs],POk,StableP, HwOk, AllocatedHw) :-
    reasoningStep(Ps, AppId, AllocHW,KOs,POk,StableP, HwOk, AllocatedHw).
reasoningStep([],_,_,[],P,P,Hw,Hw). 


% newServices(appCR, adaptive, [on(s1, full, n1)], NewServices).  NewServices = [(s2,_)]
newServices(AppId, AppVersion, P, NewServices) :-  
    application((AppId,AppVersion), MELsInApp),
    findall((Mel,_), (member((Mel,_), MELsInApp), \+ member(on(Mel,_,_), P)), NewServices).

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
% footprint([(n8, 7)], 0, E, 0, CO2)
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
% placement(arApp, adaptive, 110, Placement, PlacementProfit, [(edge42, 2)], []).

% placement(arApp, adaptive, 110, Placement, PlacementProfit, [], AllocatedHW).
% placement(app1, adaptive, 110, Placement, PlacementProfit, [(arApp,[(edge42,1), (cloud42,88)])], AllocatedHW).
placement(AppName, AppVersion, MaxCost, Placement, TotProfit, OldHw, AllocatedHW) :-
    application((AppName, AppVersion), MELs),
    melPlacementOK(AppName, MELs, Placement, 0, TotProfit, MaxCost, OldHw, AllocatedHW),
    findall(mel2mel(M1,M2,Latency), mel2mel_in_placement(M1,M2,Latency,Placement), FlowConstraints),
    flowsOK(FlowConstraints, Placement).

% placementCR(ServicesToPlace, MaxCost, Placement, PlacementProfit, AllocatedHW, OldHw, OkHws, OkPlacement),([(n1, 1), (n2, 2)]).
% placementCR([(s1,_)],999,TotProfit, [(appCR, [(n1, 1), (n2, 2)])], AllocatedHW, [on(s2, full, n2)], Placement).
placementCR(ServicesToPlace, AppName, MaxCost, TotProfit, OldHw, AllocatedHW, OkPlacement, Placement) :-
    melPlacementOK(AppName, ServicesToPlace, NewPartialP, 0, TotProfit, MaxCost, OldHw, AllocatedHW),
    append(NewPartialP, OkPlacement, Placement),
    findall(mel2mel(M1,M2,Latency), mel2mel_in_placement(M1,M2,Latency,Placement), FlowConstraints),
    flowsOK(FlowConstraints, Placement).

% oldOkP([(n2,1)], [on(s1, full, n1), on(s2, full, n2)], [on(s1, full, n2)], Placement).
oldOkP([], _, _, []).
oldOkP([(N,H)|Rest], OldP, ToFixPlacement, [on(Mel, Version, N) | Placement]) :-
    oldOkP(Rest, OldP, ToFixPlacement, Placement),
    (member(on(Mel, _, N), OldP), \+ member(on(Mel, _, N), ToFixPlacement), mel((Mel, Version), _, H, _)).


% evalPacements ranks a list of placements
evalPlacements(_, _, _, [], []).
evalPlacements(AppName, AppVersion, PreferredMelVersion, [(Placement,Cost)], [[_, VersionCompliance, Cost, Placement]]):-
    application((AppName, AppVersion), NewMels), length(NewMels, NMels), 
    findall(S, member(on(S, PreferredMelVersion, _), Placement), Ls), length(Ls, NPreferredVersionMels),
    VersionCompliance is div(100*NPreferredVersionMels,NMels).
    evalPlacements(AppName, AppVersion, PreferredMelVersion, Placements, EvaluatedPlacements):-
    length(Placements, L), L>1, 
    application((AppName, AppVersion), NewMels), length(NewMels, NMels),
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
