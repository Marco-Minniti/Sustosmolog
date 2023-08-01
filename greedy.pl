:- use_module(library(lists)).
:-['core'].
:-['example'].


% Finds the best candidate placement via heuristic search, returns CPU time.
% example query:
%   go(arApp, adaptive, full, 100, P, VC, C, Time). 
go(Application, V, PrefVersion, CapCost, Placement, VersionCompliance, Cost, Time) :-
    statistics(cputime, Start),
    h_placement(Application, V, PrefVersion, CapCost,   Placement, VersionCompliance, Cost),
    statistics(cputime, Stop), Time is Stop - Start.

h_placement(Application, V, PrefVersion, CapCost,  Placement, VersionCompliance, Cost) :-
    application((Application, V), Mels),
    setInitalState(InitialState),
    preprocessing(Mels, Compatibles),            
    h_melPlacement(Mels, Compatibles, PrefVersion, CapCost, InitialState, FinalState),
    ( (dif(Placement,[]), extract(FinalState, Placement, VersionCompliance, Cost)) ; Placement=none ), !.
    
%% Sorts the list of Mels by increasing number of compatible nodes.
preprocessing(Mels, Compatibles) :-
    findall(N, ( member((M,V),Mels), compatible(M,V,N) ),  Compatibles).

compatible(M,V,((M,V),N,Cost) ) :- 
	mel((M, V), SW_Reqs, HW_Reqs, Thing_Reqs),
	node(N, SW_Caps, (HW_Caps, C), Thing_Caps),
    swReqsOK(SW_Reqs, SW_Caps, SWCost),
    thingReqsOK(Thing_Reqs, Thing_Caps, ThingCost),
    HW_Reqs =< HW_Caps, HwCost is HW_Reqs * C,
    Cost is SWCost + ThingCost + HwCost.

% Search state have the form: f(AplusB,((A,Atot,Amatched),(B,Bcost,Bmax,Bmin),AllocHW,P))
setInitalState(f(0, ((0,0,0),(0,0,0,0),[],[]))).
extract(f(_, ((A,_,_),(_,Bcost,_,_),_,Placement)), Placement, A, Bcost). 

% Expands the best state with a +1 look-ahead.
% Considers states 
h_melPlacement([],_, _, _, X, X) :- getPlacement(X,Placement), checkFlows(Placement).
h_melPlacement(Mels, Compatibles, PrefVersion, CapCost, State, NewState) :-
    getAllocHW(State, AllocHW), 
    findall((NodeCost,MV,N,NewAllocHW), 
        ( member(MV,Mels), member((MV, N, NodeCost), Compatibles), placementOK(MV, N, AllocHW, NewAllocHW) ), List),
    max_and_min(List,MaxCost,MinCost), 
    lookAhead(List,PrefVersion,MaxCost,MinCost,State,NewList1), 
    sort(NewList1,Tmp), reverse(Tmp, NewList2), % NewList2 is sorted by decreasing ranking estimates
    increasingOptions(Mels,NewList1,Options), % from the (M,V) with less options to the one with more
    member((_,BestMV),Options), member((NextState,BestMV), NewList2), 
    NextState=f(_,(_,(_,Bcost,_,_),_,_)), Bcost =< CapCost, 
    getPlacement(NextState,Placement), checkFlows(Placement),
    updateMels(BestMV,Mels,NewMels), 
    h_melPlacement(NewMels, Compatibles, PrefVersion, CapCost, NextState, NewState).

placementOK((M, Version), N, AllocatedHW, NewAllocatedHW) :- 
	mel((M, Version), _, HW_Reqs, _),
	node(N, _, HW_Caps, _),
    hwReqsOK(HW_Reqs, HW_Caps, N, AllocatedHW, NewAllocatedHW).

hwReqsOK(HW_Reqs, (HW_Cap, _), N, [], [(N,HW_Reqs)] ) :- 
    HW_Reqs =< HW_Cap.
hwReqsOK(HW_Reqs, (HW_Cap, _), N, [(N,A)|As], [(N,NewA)|As]) :-
    HW_Reqs + A =< HW_Cap, NewA is A + HW_Reqs.
hwReqsOK(HW_Reqs, HW_Caps, N, [(N1,A1)|As], [(N1,A1)|NewAs]) :-
    dif(N,N1), hwReqsOK(HW_Reqs, HW_Caps, N, As, NewAs).

increasingOptions(Mels,List,SortedL):- 
    findOptionsPerMel(Mels, List, Options),
    sort(1, @=<, Options, SortedL).

findOptionsPerMel([],_,[]).
findOptionsPerMel([(M,V)|MVs], States, [(Len,M,V)|Opts]) :-
    findall( (M,V), member( (_,M,V) , States ) , List ),
    length(List,Len), 
    findOptionsPerMel(MVs, States, Opts).

checkFlows(Placement) :-
    findall(mel2mel(M1,M2,Latency), mel2mel_in_placement(M1,M2,Latency,Placement), FlowConstraints),
    flowsOK(FlowConstraints, Placement).

getAllocHW(f(_,(_,_,AllocHW,_)),AllocHW).
getPlacement(f(_,(_,_,_,Placement)),Placement).

max_and_min([(Cost,_,_,_)|Rest], MaxCost, MinCost) :- 
    length(Rest,L),L>0,
    max_and_min(Rest, RestMinCost, RestMaxCost),
    ((Cost =< RestMinCost, MinCost is Cost); (Cost > RestMinCost, MinCost is RestMinCost)),
    ((Cost >= RestMaxCost, MaxCost is Cost); (Cost < RestMaxCost, MaxCost is RestMaxCost)).
max_and_min([(Cost,_,_,_)], Cost, Cost). 

lookAhead([],_,_,_,_,[]).
lookAhead([(NodeCost,MV,N,NewAllocHW)|L],PrefVersion,MaxCost,MinCost,State,[(NewState,MV)|NewL]) :- 
    updateState((NodeCost,MV,N,NewAllocHW), PrefVersion,MaxCost,MinCost,State,NewState),
    lookAhead(L,PrefVersion,MaxCost,MinCost,State,NewL). 

updateState((NodeCost,(M,V),N,NewAllocHW), PrefVersion,MaxCost,MinCost, 
                                                    f(_,        (A_info,   B_info,   _,         P)), 
                                                    f(NewAplusB,(NewA_info,NewB_info,NewAllocHW,[on(M,V,N)|P]))) :-
        updateAInfo((M,V), N, PrefVersion, A_info, NewA_info), 
        updateBInfo((M,V), N, NodeCost, MaxCost,MinCost, A_info, B_info, NewB_info),
        updateRanking(NewA_info,NewB_info,NewAplusB).

updateAInfo(MV, _, PrefVersion, (_,Atot,Amatched), (NewA,NewAtot,NewAmatched)) :- 
        NewAtot is Atot+1, setAmatched(MV, PrefVersion, Amatched, NewAmatched), NewA is div(100*NewAmatched,NewAtot).
setAmatched((_,V), V, Amatched, NewAmatched) :- NewAmatched is Amatched+1.
setAmatched((_,V), V1, Amatched, Amatched) :- dif(V,V1).

updateBInfo(_, _, NodeCost, MaxCost, MinCost, (_,Atot,_), (_,Bcost,Bmax,Bmin), (NewB,NewBcost,NewBmax,NewBmin)) :-
    NewBmax is Bmax+MaxCost, NewBmin is Bmin+MinCost, NewBcost is Bcost+NodeCost,
    ( ( ( Atot =< 1 ; NewBmax - NewBmin = 0), NewB is 100);
      ( Atot>1, NewBmax - NewBmin > 0, NewB is div((100*(NewBmax-NewBcost)),(NewBmax-NewBmin)) ) ).

updateRanking((A,_,_),(B,_,_),AplusB) :- AplusB is A+B.

updateMels(MV,[MV|L],L).
updateMels(MV,[MV1|L],[MV1|NewL]) :- dif(MV,MV1), updateMels(MV,L,NewL).