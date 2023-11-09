% set_prolog_flag(answer_write_options, [quoted(true), portray(true), max_depth(0), spacing(next_argument)]).

:- set_prolog_flag(stack_limit, 16 000 000 000).
:- set_prolog_flag(last_call_optimisation, true).

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- consult('core').
%:- ['example'].
:- consult('infras/infra4d').




%-----------------------------------------------------------------------------------------------

% Request format: request(AppId, AppVersion, PreferredMELVersion, MaxCost). 
 
% request(arApp, adaptive, 116). 
% request(vrApp, adaptive, 500).
% request(appCo2, adaptive, 999).

 
%-----------------------------------------------------------------------------------------------

:- dynamic deploymentsInfos/4.

:- dynamic deploymentsDetails/1. 



best(Infos, Details) :-
    findRequests(As),
    oldAllocation(OldHw),
    toBeProcessed(As, OldHw, AllRequests), 
    avaiableSolutions(AllRequests, OldHw, SortedSols),
    updateDeploymentState(SortedSols, _, _), 
    writeOnFile('output', Infos, Details).


findRequests(As):- findall(( AppId, AppVersion, MaxCost), request(AppId, AppVersion, MaxCost), As).

oldAllocation(FilteredHw) :-
    (deploymentsDetails(OldHw); \+ deploymentsDetails(_), OldHw = []),
    filterDeploymentsDetails(OldHw, FilteredHw).

toBeProcessed(As, OldHw, AllRequests):-
    once(verifyUpdMel(As, OldHw, _, NewRequests)), 
    append(NewRequests, As,  AllRequests).

avaiableSolutions(AllRequests, OldHw, SortedSols) :-
    findall(sol(Profit,Placements, HwUsed),  ( place([], AllRequests, Placements, OldHw, HwUsed), profit(Placements, Profit) ), Solutions), 
    byDecreasingProfit(Solutions, SortedSols).

verifyUpdMel([], _, O, O).
verifyUpdMel([(A, _, _)|As], FilteredHw, NewRequests, Out) :- 
    application((A,_), MelsA),
    findNodes(MelsA, [], Nodes),
    (deploymentsInfos(_,Ps,_,_); \+ deploymentsInfos(_,_,_,_), Ps = []),
    findall(AppName, (member(N, Nodes), member(s(AppName, Mels), Ps), member(on(_,_,N), Mels)), Replace),
    removeOccurrences(Replace, [], NewReplace),
    equalValue(NewReplace, Ps, FilteredHw, [], NewRequests),
    verifyUpdMel(As, FilteredHw, NewRequests, Out).

removeOccurrences([], O, O).
removeOccurrences([A|R], Tmp, Out) :-
    (\+ member(A, Tmp), removeOccurrences(R, [A|Tmp], Out); removeOccurrences(R, Tmp, Out)).

equalValue([], _, _, N, N).
equalValue([App|Rest], Ps, FilteredHw, New, NewRequests) :-
    findall(Hw , (member(s(App, Mels), Ps), member(on(M,V,_), Mels), mel((M, V), _, Hw, _)) , Hws), sum_list(Hws, HwsApp), 
    member((App, H), FilteredHw), 
    findall(OldH , (member((_, OldH), H)), OldHs), sum_list(OldHs, OldHws),
    (HwsApp \= OldHws, \+member(((_,_), App, _, _),New), equalValue(Rest, Ps, FilteredHw, [(App, adaptive, 999)|New], NewRequests); equalValue(Rest, Ps, FilteredHw, New, NewRequests)).
    

findNodes([], N, N).
findNodes([(M,_)|Rest], Nodes, Out) :- 
    mel((M, _), SW_Reqs, _, Thing_Reqs),
	node(N, SW_Caps, _, Thing_Caps),
    swReqsOK(SW_Reqs, SW_Caps, _),
    thingReqsOK(Thing_Reqs, Thing_Caps, _),
    (member(N, Nodes), findNodes(Rest, Nodes, Out); findNodes(Rest, [N | Nodes], Out)).
    


writeOnFile(NomeFile, Infos, Details) :-
    open(NomeFile, append, File),
    deploymentsInfos(X, Y, Z, A), write(File, X), write(File, ','), write(File, Y), write(File, ','), write(File, Z), write(File, ','), write(File, A), 
    deploymentsDetails(B), write(File, '\n'), write(File, B), write(File, '\n'),
    close(File),
    Infos = [X,Y,Z,A],
    Details = B.

byDecreasingProfit(Sols, SortedSols) :- sort(Sols, Tmp), reverse(Tmp, SortedSols).


filterDeploymentsDetails([],[]).
filterDeploymentsDetails([(AppId, Hw, _, _)|Rest], [(AppId,Hw)|FilteredHw]) :-
    filterDeploymentsDetails(Rest, FilteredHw).

updateDeploymentState(Reversed, BestPlacements, BestProfit) :- updateDeploymentState(Reversed, [], 0, BestPlacements, BestProfit).
updateDeploymentState([], BestPlacements, BestProfit, BestPlacements, BestProfit).
updateDeploymentState([sol(Profit,Placements,AllocatedHW)|_], _, _, BestPlacements, BestProfit) :-
    greenCheck(AllocatedHW, [], SustParam, 0),
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


profit([s(_,_,ProfitP,_)|Ps], Profit) :-
    profit(Ps,TmpProfit),
    Profit is ProfitP + TmpProfit.
profit([], 0).


place(Placements, [A|As], NewPlacements, OldHw, HwUsed) :-
    place_app(Placements, A, TempPlacements, OldHw, NewHw),
    place(TempPlacements, As, NewPlacements, NewHw, HwUsed). 
place(Placements, [_|As], NewPlacements, OldHw, HwUsed) :-
    place(Placements, As, NewPlacements, OldHw, HwUsed). 
place(Placements, [], Placements, OldHw, OldHw). 



place_app(Placements, (AppId, AppVersion, MaxCost), [s(AppId, Placement, PlacementProfit, AllocatedHW)|Placements], OldHw, AllocatedHW) :-
    deploymentsInfos(_, Ps, _, _), member(s(AppId,P), Ps), % REPLACE APP
    newServices(AppId, AppVersion, P, NewServices), 
    select((AppId,_), OldHw, (AppId,[]), OtherHws),
    once(reasoningStep(P, AppId, OtherHws, NotOkServices, [], OkPlacement, OkHws, 0, OkProfit, MaxCost)),
    append(NewServices,NotOkServices,ServicesToPlace),
    (length(ServicesToPlace, Lenght), Lenght > 0; length(P, Lp), length(OkPlacement, Lok), Lok<Lp),
    placementCR(ServicesToPlace, AppId, MaxCost, PlacementProfit, OkHws, AllocatedHW, OkPlacement, Placement, OkProfit).
place_app(Placements, (AppId, AppVersion, MaxCost), [s(AppId, Placement, PlacementProfit, AllocatedHW)|Placements], OldHw, AllocatedHW) :-
    (member((AppId,_), OldHw), select((AppId,_), OldHw, (AppId,[]), OtherHws); OtherHws=OldHw),
    placement(AppId, AppVersion, MaxCost, Placement, PlacementProfit, OtherHws, AllocatedHW).


   
reasoningStep([on(S,_,_)|Ps], AppId, AllocHW, KOs, POk, StableP, AllocatedHw, OkProfit, TotalProfit, MaxCost) :-
    (\+ mel((S,_),_,_,_); application((AppId,_), MELsInApp), \+ member((S,_), MELsInApp)),
    reasoningStep(Ps, AppId, AllocHW, KOs, POk, StableP, AllocatedHw, OkProfit, TotalProfit, MaxCost).
reasoningStep([on(S,V,_)|Ps], AppId, AllocHW, [(S,_)|KOs], POk, StableP, AllocatedHw, OkProfit, TotalProfit, MaxCost) :-
    \+ mel((S,V),_,_,_), mel((S,_),_,_,_),
    reasoningStep(Ps, AppId, AllocHW, KOs, POk, StableP, AllocatedHw, OkProfit, TotalProfit, MaxCost).
reasoningStep([on(S,V,N)|Ps], AppId, AllocHW, KOs, POk, StableP, AllocatedHw, OkProfit, TotalProfit, MaxCost) :- 
    placementOK(AppId, (S, V), N, Profit, AllocHW, NewAllocatedHW),
    NewProfit is OkProfit + Profit, NewProfit =< MaxCost,
    reasoningStep(Ps, AppId, NewAllocatedHW, KOs, [on(S,V,N)|POk], StableP, AllocatedHw, NewProfit, TotalProfit, MaxCost).
reasoningStep([on(S,_,N)|Ps], AppId, AllocHW,[(S,_)|KOs],POk,StableP, AllocatedHw, OkProfit, TotalProfit, MaxCost) :-
    \+ placementOK(AppId, (S, _), N, _, AllocHW, _),
    reasoningStep(Ps, AppId, AllocHW,KOs,POk,StableP, AllocatedHw, OkProfit, TotalProfit, MaxCost).
reasoningStep([],_,Hw,[],P,P,Hw,Pr,Pr,_). 


newServices(AppId, AppVersion, P, NewServices) :-  
    application((AppId,AppVersion), MELsInApp),
    findall((Mel,_), (member((Mel,_), MELsInApp), \+ member(on(Mel,_,_), P)), NewServices).


greenCheck([], TmpList, TmpList, _).
greenCheck([(AppId, HwUsed)|Rest], TmpList, SustParam, ContCo2) :-
    byDecreasingProfit(HwUsed, Shw), 
    footprint(Shw, 0, E, 0, CO2),
    targetsOk(E, CO2, ContCo2, Out),
    greenCheck(Rest, [(AppId, [(co2, CO2), (energy, E)])|TmpList], SustParam, Out).



targetsOk(E, CO2, OldCo2, Out) :-
    ( deploymentsInfos(_, _, _, OldSustParam), member((co2, Co2Asserted), OldSustParam), member((energy, EnergyAsserted), OldSustParam); \+ deploymentsInfos(_, _, _, _), Co2Asserted is 0, EnergyAsserted is 0 ),
    target(co2, Trgt),
    Danger is Trgt * 0.80,
    ( Co2Asserted > Danger,  CMax is Danger; CMax is Trgt), !,
    Out is CO2 + OldCo2,
    Out =< CMax,
    target(energy, EMax), E + EnergyAsserted =< EMax.


carbonIntensity(N, C) :- 
    energyMix(N, Sources),
    avgCI(Sources, C).

avgCI(S, C) :- avgCI(S, 0, Ctot), dif(Ctot, 0), length(S, L), C is Ctot/L.
avgCI(S, 0) :- avgCI(S, 0, 0).

avgCI([(S, P)|Ss], OldC, NewC) :-
    co2(S, Cs),
    TmpC is OldC + (P * Cs),
    avgCI(Ss, TmpC, NewC).
avgCI([], C, C).



footprint([(N,H)|As], OldW, NewW, OldCO2, NewCO2) :-
    energyConsumption(N, EnergyXHw), TmpW is OldW + (H * EnergyXHw),
    carbonIntensity(N, C), TmpCO2 is OldCO2 + C * (H * EnergyXHw), 
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

go( AppName, AppVersion, PreferredMelVersion, MaxCost, VC, C, Best, Time) :-
statistics(cputime, Start),
goForBest( AppName, AppVersion, PreferredMelVersion, MaxCost, BestPlacement),
statistics(cputime, Stop), Time is Stop - Start,
nth0(1,BestPlacement,VC), nth0(2,BestPlacement,C), nth0(3,BestPlacement,Best).


% goForBest(arApp, adaptive, full, 110, BestPlacement, PlacementsHw, []).
% goForBest(testApp, adaptive, full, 999, BestPlacement, PlacementsHw, []).
goForBest( AppName, AppVersion, PreferredMelVersion, MaxCost, BestPlacement, PlacementsHw, OldHw) :-
    findall((Placement, PlacementProfit, AllocatedHW), placement(AppName, AppVersion, MaxCost, Placement, PlacementProfit, AllocatedHW, OldHw), PlacementsHw),
    maplist(extractPlacements, PlacementsHw, Placements),
    evalPlacements(AppName, AppVersion, PreferredMelVersion, Placements, EvaluatedPlacements),
    best(EvaluatedPlacements,BestPlacement).

extractPlacements((Placement, PlacementProfit, _), (Placement, PlacementProfit)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

placement(AppName, AppVersion, MaxCost, Placement, TotProfit, OldHw, AllocatedHW) :-
    application((AppName, AppVersion), MELs),
    melPlacementOK(AppName, MELs, Placement, 0, TotProfit, MaxCost, OldHw, AllocatedHW),
    findall(mel2mel(M1,M2,Latency), mel2mel_in_placement(M1,M2,Latency,Placement), FlowConstraints),
    flowsOK(FlowConstraints, Placement).

                                                   
placementCR(ServicesToPlace, AppName, MaxCost, TotProfit, OldHw, AllocatedHW, OkPlacement, Placement, OkProfit) :-
    melPlacementOK(AppName, ServicesToPlace, NewPartialP, OkProfit, TotProfit, MaxCost, OldHw, AllocatedHW),
    append(NewPartialP, OkPlacement, Placement),
    findall(mel2mel(M1,M2,Latency), mel2mel_in_placement(M1,M2,Latency,Placement), FlowConstraints),
    flowsOK(FlowConstraints, Placement).


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

% once(loadInfrastructure()).
loadInfrastructure() :-
    open('infra.pl', read, Str),
    %open('./infras/1', read, Str),
    (retractall(node(_,_,_,_)), retractall(link(_,_,_)), retractall(energyConsumption(_,_)), retractall(energyMix(_,_)); true),
    readAndAssert(Str).

readAndAssert(Str) :-
    read(Str, X), (X == end_of_file -> close(Str) ; assert(X), readAndAssert(Str)).