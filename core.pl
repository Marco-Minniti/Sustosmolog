
% melPlacementOK([(s1, _)], P, [(n2, 1)], 0, TotalProfit, 999, FinalAllocatedHW, [])
melPlacementOK(_, [], [], PlacementProfit, PlacementProfit, _, AllocatedHW, AllocatedHW).
melPlacementOK(AppName, [(M, Version)| Ss], [on(M, Version, N)|P], ProfitUpToNow, TotalProfit, MaxCost, OldHw, FinalAllocatedHW) :-
    placementOK(AppName, (M, Version), N, NodeProfit, OldHw, NewAllocatedHW),
    NewProfit is ProfitUpToNow + NodeProfit,
    NewProfit =< MaxCost,
    melPlacementOK(AppName, Ss, P, NewProfit, TotalProfit, MaxCost, NewAllocatedHW, FinalAllocatedHW).



% placementOK(appCR, (s2, full), n2, _, (appCR, [(n2, 6), (n1, 4)]), _).
% placementOK(arApp, (usersData, full), cloud42, _, [(arApp, [(edge42, 2), (cloud42, 80)])], _).
% placementOK(arApp, (movementProcessing, _), N, _, [], _).
placementOK(AppName, (M, Version), N, NodeProfit, OldHw, NewAllocatedHW) :- 
	mel((M, Version), SW_Reqs, HW_Reqs, Thing_Reqs),
	node(N, SW_Caps, HW_Caps, Thing_Caps),
    swReqsOK(SW_Reqs, SW_Caps, NodeSwProfit),
    thingReqsOK(Thing_Reqs, Thing_Caps, NodeThingProfit),
    hwReqsOK(AppName, HW_Reqs, HW_Caps, N, NodeHwProfit, OldHw, NewAllocatedHW),
    NodeProfit is NodeSwProfit + NodeThingProfit + NodeHwProfit. 
    
% Checks software requirements and returns their cost if they can be satisfied.
swReqsOK(SW_Reqs, SW_Caps, Profit) :- costIfCapsOK(SW_Reqs, SW_Caps, Profit).

% Checks IoT requirements and returns their cost if they can be satisfied.
thingReqsOK(T_Reqs, T_Caps, Profit) :- costIfCapsOK(T_Reqs, T_Caps, Profit).

% Checks capabilities and returns their cost if they can be satisfied.
costIfCapsOK([], _, 0).
costIfCapsOK([Cap| CWs], Caps, TotalProfit) :-
   member((Cap, Profit), Caps), % controllo se la capability è presente e in tal caso prendo il costo
   costIfCapsOK(CWs, Caps, Rest),
   TotalProfit is Profit + Rest.


% hwReqsOK(appCR, 6, (5, 1), n1, _, [(appCR, [(n2, 6), (n1, 4)])], []).
% hwReqsOK(arApp, 64, (100, 1), cloud42, _, [(arApp, [(edge42, 2), (cloud42, 16)])], _).
% hwReqsOK(appCR, 6, (5, 1), n1, _74296, [(arApp, [(edge42, 2), (cloud42, 80)]), (appCR, [])], NewAllocatedHW).
% hwReqsOK(testApp, 4, (6, 3), edge42, NewProfit, [(testApp, [(cloud42, 20)]), (arApp, [(edge42, 2), (cloud42, 80)])], NewAllocatedHW).
% hwReqsOK(arApp, 64, (128, 3), node2, NewProfit, [], NewAllocatedHW).
hwReqsOK(AppName, HWReqs, (HWCap, Profit), N, NewProfit, OldHw, NewAllocatedHW) :-
    findall(Value, (member((_, Hws), OldHw), member((N, Value), Hws)), HwsAtN),
    sum_list(HwsAtN, Sum),
    HWCap - Sum >= HWReqs,
    
    updatedAllocation(AppName, N, HWReqs, OldHw, NewAllocatedHW),
    updatedCost(HWReqs, Profit, NewProfit).

updatedCost(HWReqs, Profit, NewProfit) :- NewProfit is HWReqs * Profit.
updatedAllocation(AppName, N, HWReqs, OldHw, [(AppName, [(N,HWReqs)])|OldHw]) :- 
    \+ member((AppName,_), OldHw).
updatedAllocation(AppName, N, HWReqs, OldHw, NewAllocatedHW) :- 
    member((AppName,HwList), OldHw), \+ member((N,_), HwList),
    append([(N,HWReqs)], HwList, NewHwList),
    select((AppName,HwList), OldHw, (AppName,NewHwList), NewAllocatedHW).
updatedAllocation(AppName, N, HWReqs, OldHw, NewAllocatedHW) :- 
    member((AppName,HwList), OldHw), member((N,PreviouslyUsedHwAtN), HwList),
    NewUsedHwAtN is PreviouslyUsedHwAtN + HWReqs,
    select((N,PreviouslyUsedHwAtN), HwList, (N,NewUsedHwAtN), NewHwList),
    select((AppName,HwList), OldHw, (AppName,NewHwList), NewAllocatedHW).

% Retrieve all s2s of already placed MELs
mel2mel_in_placement(S1, S2, Latency, P) :-
    mel2mel(S1, S2, Latency),
    subset([on(S1, _, _), on(S2, _, _)], P).

% Checks all latency between MELs.
%flowsOK([mel2mel(usersData, videoStorage, 700), mel2mel(videoStorage, movementProcessing, 300), mel2mel(movementProcessing, arDriver, 200)], [on(videoStorage, full, node1), on(movementProcessing, full, node1), on(arDriver, full, node6), on(usersData, full, node8)])

flowsOK([], _).
flowsOK([mel2mel(S1,S2,_)|SFs], P) :-
    member(on(S1, _, N), P), member(on(S2, _, N), P),
    flowsOK(SFs, P).
flowsOK([mel2mel(S1,S2,L)|SFs], P) :-
    member(on(S1, _, N1), P), member(on(S2, _, N2), P), dif(N1,N2),
    e2elink(N1,N2,Latency), 
    Latency =< L,
    flowsOK(SFs, P).

%Suceeds if there exists a link between N1 and N2 or N2 and N1.
e2eLink(N1,N2,L) :-
    link(N1,N2,L).
e2elink(N1,N2,L) :-
    link(N2,N1,L).