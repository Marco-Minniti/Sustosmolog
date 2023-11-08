


ESEMPIO 1: PIAZZAMENTO MULTIPLO DI APPLICAZIONI
best(Infos, Details).

Infos = [417, [s(arApp, [on(usersData, full, n2), on(videoStorage, medium, n6), on(movementProcessing, full, n2), on(arDriver, light, n5)]), s(vrApp, [on(audioDriver, full, n6), on(cameraRecognition, full, n5), on(dynamicSensor, full, n6), on(..., ..., ...)])], [(n2, 72), (n6, 88), (n5, 5), (n4, 40)], [(energy, 527), (co2, 374.365)]],
Details = [(vrApp, [(n4, 40), (n5, 4), (n6, 80)], 301, [(co2, 243.98000000000002), (energy, 292)]), (arApp, [(n5, 1), (n6, 8), (n2, 72)], 116, [(co2, 130.38500000000002), (energy, 235)])].

Da notare che vengono rispettati i vincoli di usersData in versione full (data la dichiarazione application((arApp, adaptive), [(usersData,full), (videoStorage,_), (movementProcessing,_), (arDriver,_)]))
Ed inoltre viene trovata una soluzione rispettando il costo massimo per il cliente cioè 116.

------------------------------------------------------------------------------------------------
ESEMPIO 2: CONTINUOUS REASONING

Prendendo l'esito dell'esempio 1:
417,[s(arApp,[on(usersData,full,n2),on(videoStorage,medium,n6),on(movementProcessing,full,n2),on(arDriver,light,n5)]),s(vrApp,[on(audioDriver,full,n6),on(cameraRecognition,full,n5),on(dynamicSensor,full,n6),on(standbyDriver,full,n4)])],[(n2,72),(n6,88),(n5,5),(n4,40)],[(energy,527),(co2,374.365)]
[(vrApp,[(n4,40),(n5,4),(n6,80)],301,[(co2,243.98000000000002),(energy,292)]),(arApp,[(n5,1),(n6,8),(n2,72)],116,[(co2,130.38500000000002),(energy,235)])]

Con l'attuale KB, possiamo vedere che on(standbyDriver,full,n4) di VrApp è su n4, mentre di ArApp abbiamo che on(videoStorage,medium,n6),on(arDriver,light,n5)

Cambiando la KB e sostituendo a quelli già esistenti:
mel((cameraRecognition,full), [gcc,caffe], 6, [phone, lightSensor]). 
mel((standbyDriver,full), [docker], 20, []).

Otteniamo una ricollocazione dei mel: 
393,[s(arApp,[on(usersData,full,n2),on(videoStorage,medium,n4),on(movementProcessing,light,n2),on(arDriver,medium,n3)]),s(vrApp,[on(audioDriver,full,n6),on(cameraRecognition,full,n5),on(dynamicSensor,full,n6),on(standbyDriver,full,n6)])],[(n2,66),(n4,8),(n3,2)],[(energy,442),(co2,344.16066666666666)]
[(vrApp,[(n5,6),(n6,100)],278,[(co2,229.27000000000004),(energy,218)]),(arApp,[(n3,2),(n4,8),(n2,66)],115,[(co2,114.89066666666666),(energy,224)])]

Dalla quale possiamo anche notare che VrApp, avendo un margine maggiore di profitto viene collocata sui nodi n5,n6 che sono i più profittevoli. ArApp invece potendo arrivare ad un massimo di 116, su quelli meno prifittevoli. 


------------------------------------------------------------------------------------------------
ESEMPIO 3: MODIFICA PIAZZENTO GIA' ESISTENTE PER SODDISFARE NUOVA RICHIESTA
Faccio un primo piazzamento di arApp:
request(arApp, adaptive, 116).

Infos = [116, [s(arApp, [on(usersData, full, n2), on(videoStorage, medium, n6), on(movementProcessing, light, n6), on(arDriver, light, n3)])], [(n2, 64), (n6, 10), (n3, 1)], [(energy, 213), (co2, 121.16033333333333)]],
Details = [(arApp, [(n3, 1), (n6, 10), (n2, 64)], 116, [(co2, 121.16033333333333), (energy, 213)])].

Piazzo vrApp e ripizazzo arApp per soddisfare entrambe le richieste:
request(arApp, adaptive, 116). 
request(vrApp, adaptive, 500).

Infos = [417, [s(vrApp, [on(audioDriver, full, n6), on(cameraRecognition, full, n5), on(dynamicSensor, full, n6), on(standbyDriver, full, n4)]), s(arApp, [on(usersData, full, n2), on(videoStorage, medium, n6), on(movementProcessing, full, n2), on(..., ..., ...)])], [(n2, 72), (n6, 88), (n5, 5)], [(energy, 527), (co2, 374.365)]],
Details = [(arApp, [(n5, 1), (n6, 8), (n2, 72)], 116, [(co2, 130.38500000000002), (energy, 235)]), (vrApp, [(n4, 40), (n5, 4), (n6, 80)], 301, [(co2, 243.98000000000002), (energy, 292)])]

------------------------------------------------------------------------------------------------
ESEMPIO 4
Con la KB originaria:

request(vrApp, adaptive, 116).

Infos = [301, [s(vrApp, [on(audioDriver, full, n6), on(cameraRecognition, full, n5), on(dynamicSensor, full, n6), on(standbyDriver, full, n4)])], [(n6, 80), (n5, 4), (n4, 40)], [(energy, 292), (co2, 243.98000000000002)]],
Details = [(vrApp, [(n4, 40), (n5, 4), (n6, 80)], 301, [(co2, 243.98000000000002), (energy, 292)])].

-> usa on(audioDriver, full, n6)

• Se.. commento nella KB:     % mel((audioDriver,full), [docker], 40, []). 
userà correttamente:  on(audioDriver, medium, n6)

• Se.. rimuovo (audioDriver,full) da application((vrApp, adaptive), [(audioDriver,_), (cameraRecognition,_), (dynamicSensor,_), (standbyDriver,_)]).
Ottengo un piazzamento valido

• Se... aggiugo un nuovo servizio (s1,full): application((vrApp, adaptive), [(audioDriver,_), (cameraRecognition,_), (dynamicSensor,_), (standbyDriver,_), (s1,full)]).
Il reasoning step lo trova e soddisfa la richiesta e ottengo un piazzamento valido

------------------------------------------------------------------------------------------------
ESEMPIO 5

<img src="/img/co2.jpeg" width="300">

Introduco una nuova (appCo2) che fa in modo di posizionarsi all'interno della "Danger Zone" dell Co2.

 request(vrApp, adaptive, 500).
 request(appCo2, adaptive, 999).

Il risultato di questo piazzamento è che ho raggiunto una quota di Co2 all'interno della "danger zone" come possiamo vedere dalla co2 complessiva  (co2, 942.48).

Infos = [936, [s(vrApp, [on(audioDriver, full, n4), on(cameraRecognition, full, n5), on(dynamicSensor, full, n6), on(standbyDriver, full, n6)]), s(appCo2, [on(sCo2, full, nCo2)])], [(n4, 40), (n5, 4), (n6, 80), (nCo2, 635)], [(energy, 927), (co2, 942.48)]],
Details = [(appCo2, [(nCo2, 635)], 635, [(co2, 698.5), (energy, 635)]), (vrApp, [(n6, 80), (n5, 4), (n4, 40)], 301, [(co2, 243.98000000000002), (energy, 292)])].

Un nuovo replacement delle app, fa si che vengano ridotte le guise dei mel di vrApp (in quanto appCo2 ammette un solo placement).
Il risultato è una perfetta collocazione appena al di sotto della "danger zone". (co2, 799.062)

Infos = [843, [s(vrApp, [on(audioDriver, medium, n4), on(cameraRecognition, full, n5), on(dynamicSensor, medium, n3), on(standbyDriver, full, n4)]), s(appCo2, [on(sCo2, full, nCo2)])], [(n4, 60), (n5, 4), (n3, 6)], [(energy, 833), (co2, 799.062)]],
Details = [(appCo2, [(nCo2, 635)], 635, [(co2, 698.5), (energy, 635)]), (vrApp, [(n3, 6), (n5, 4), (n4, 60)], 208, [(co2, 100.562), (energy, 198)])]

