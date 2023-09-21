
% melPlacementOK([(usersData, full), (videoStorage,_), (movementProcessing,_), (arDriver,_)], Placement, [], 0, TotCost, 110, AllocatedHW).
melPlacementOK([], [], AllocatedHW, PlacementProfit, PlacementProfit, _, AllocatedHW, _).
melPlacementOK([(M, Version)| Ss], [on(M, Version, N)|P], AllocatedHW, ProfitUpToNow, TotalProfit, MaxCost, FinalAllocatedHW, OldHw) :-
    placementOK((M, Version), N, AllocatedHW, NewAllocatedHW, NodeProfit, OldHw),
    NewProfit is ProfitUpToNow + NodeProfit,
    NewProfit =< MaxCost,
    melPlacementOK(Ss, P, NewAllocatedHW, NewProfit, TotalProfit, MaxCost, FinalAllocatedHW, OldHw).

% Places one MEL to a node N that can host it.
% Returns the cost of such a placement.
placementOK((M, Version), N, AllocatedHW, NewAllocatedHW, NodeProfit, OldHw) :- 
	mel((M, Version), SW_Reqs, HW_Reqs, Thing_Reqs),
	node(N, SW_Caps, HW_Caps, Thing_Caps),
    swReqsOK(SW_Reqs, SW_Caps, NodeSwProfit),
    thingReqsOK(Thing_Reqs, Thing_Caps, NodeThingProfit),
    hwReqsOK(HW_Reqs, HW_Caps, N, AllocatedHW, NewAllocatedHW, NodeHwProfit, OldHw),
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

% hwReqsOK(1, (6, 3), edge42, [(cloud42, 88), (edge42, 2)], NewAllocatedHW, NodeHwProfit).
hwReqsOK(HWReqs, (HWCap, Profit), N, AllocatedHW, NewAllocatedHW, NewProfit, OldHw) :-
    % ( usedHw(N, UsedHw); \+ usedHw(N,_), UsedHw is 0 ),
    ( deploymentsInfos(_, _, OldAllocatedHW, _), member((N, HwAsserted), OldAllocatedHW); \+ deploymentsInfos(_, _, _, _), HwAsserted is 0 ), % Hw allocato nelle vecchie sessioni
    (   (member((N,UsedHw), OldHw)) ; ( \+ member((N,_), OldHw), UsedHw is 0 ) ), % Hw delle richieste precedenti, sessione corrente
    (   (member((N,PreviouslyUsedHwAtN), AllocatedHW)) ; ( \+ member((N,_), AllocatedHW), PreviouslyUsedHwAtN is 0 ) ), % Hw della richiesta corrente
    RequiredHW is HWReqs + PreviouslyUsedHwAtN + UsedHw + HwAsserted, HWCap >= RequiredHW,
    updatedAllocation(N, HWReqs, AllocatedHW, NewAllocatedHW),
    updatedCost(HWReqs, Profit, NewProfit).

updatedCost(HWReqs, Profit, NewProfit) :- NewProfit is HWReqs * Profit.
% con questo modo di aggiornare il placement posso rimuovere tutti gli hwReqsOK2.
updatedAllocation(N, HWReqs, AllocatedHW, [(N,HWReqs)|AllocatedHW]) :- 
    \+ member((N,_), AllocatedHW).
updatedAllocation(N, HWReqs, AllocatedHW, NewAllocatedHW) :- 
    member((N,PreviouslyUsedHwAtN), AllocatedHW),
    NewUsedHwAtN is PreviouslyUsedHwAtN + HWReqs,
    select((N,PreviouslyUsedHwAtN), AllocatedHW, (N,NewUsedHwAtN), NewAllocatedHW).

% Retrieve all s2s of already placed MELs
mel2mel_in_placement(S1, S2, Latency, P) :-
    mel2mel(S1, S2, Latency),
    subset([on(S1, _, _), on(S2, _, _)], P).

% Checks all latency between MELs.
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