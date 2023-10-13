<img src="https://github.com/di-unipi-socc/osmolog/blob/master/img/logo.png" width="300">



ESEMPIO 1: PIAZZAMENTO MULTIPLO DI APPLICAZIONI
request((0,highest), app1, adaptive, full, 999).
request((0,highest), arApp, adaptive, full, 110).
Output in...
--> deploymentsInfos(X, Y, Z, A).
--> deploymentsDetails(X).
------------------------------------------------------------------------------------------------
ESEMPIO 2: CONTINUOUS REASONING
request((0,highest), appCR, adaptive, full, 999).
Con i seguenti mel: (presenti in example.pl)
mel((s1,full), [test], 4, []).
mel((s2,full), [test], 6, []).
--> Piazzamento: s1 -> n1, s2 ->n2.

Cambio requisiti mel (commenta quelli precedenti): 
 mel((s1,full), [test], 6, []).
 mel((s2,full), [test], 1, []).
 --> Modifica piazzamento già esistente, Out: s1 -> n2, s2 ->n2.
 Il tutto tenendo aggiornate le variabili dinamiche:
--> deploymentsInfos(X, Y, Z, A).
--> deploymentsDetails(X).



