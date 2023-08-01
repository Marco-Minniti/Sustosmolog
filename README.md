<img src="https://github.com/di-unipi-socc/osmolog/blob/master/img/logo.png" width="300">

A declarative solution for placing and configuring applications in Osmotic Computing settings.

Osmolog methodology is described and assessed in:

> [Stefano Forti](http://pages.di.unipi.it/forti), [Antonio Brogi](http://pages.di.unipi.it/brogi)<br>
> [**Declarative Osmotic Application Placement**](https://link.springer.com/chapter/10.1007/978-3-030-79022-6_15), <br>	
> [Next Generation Information Systems (NeGIS)](https://www.negis.polimi.it) @ [CAiSE](https://caise21.org), 2021.

If you wish to reuse source code in this repo, please consider citing the above mentioned article.

### Background and Requirements

osmolog is written in Prolog. Prolog programs are finite sets of *clauses* of the form:

```prolog
a :- b1, ... , bn.
```

stating that `a` holds when `b1` and ... and `bn` holds, where `n >= 0` and `a`, `b1` ..., `bn` are atomic literals. Clauses with empty condition are also called *facts*. Prolog variables begin with upper-case letters, lists are denoted by square brackets, and negation by `\+`.

To run osmolog, please install [SWI-Prolog](https://www.swi-prolog.org/Download.html).

## Quickstart Example

Consider the Osmotic application below from Augmented Reality.

<img src="https://github.com/di-unipi-socc/osmolog/blob/master/img/app.png" width="500">

It is made of four MicroELements (MELs), some of which exist in more than one version with different IoT, software and hardware requirements. Versions range from the less demanding `light` version (i.e. triangles) to a `medium` version (i.e. squares) to a `full` version (i.e. circles). Those three versions (or flavours) are suited for IoT, Edge and Cloud devices respectively.

Given a Cloud-IoT infrastructure, osmolog *jointly* determines a solution to these placement-related questions:

> Where to deploy each MEL composing the application?

and

> Which MEL version to deploy?

### Model

We first describe the osmolog model, following the input data contained in the file `example.pl`.

###### Application

A fully adaptive version of the application above can be specified as in:

```prolog
mel((usersData,full), [docker], 64, []).

mel((videoStorage,full), [docker], 16, []).
mel((videoStorage,medium), [docker], 8, []).

mel((movementProcessing,full), [docker], 8, []).
mel((movementProcessing,medium), [gcc, make], 4, []).

mel((arDriver,full), [docker], 4, [phone, lightSensor]).
mel((arDriver,medium), [gcc,caffe], 2, [phone, lightSensor]).
mel((arDriver,light), [gcc], 1, [phone]).

mel2mel(usersData, videoStorage, 70).
mel2mel(videoStorage, movementProcessing, 30).
mel2mel(movementProcessing, arDriver, 20).

application((arApp, adaptive), [(usersData,full), (videoStorage,_), (movementProcessing,_), (arDriver,_)]).
```

Note that `mel/4` facts denote all MEL requirements for different versions `(MelId, Version)` in terms of software requirements, hardware resources and IoT requirements. Besides, `mel2mel/3` denote latency requirements in milliseconds between application MELs. Finally, `application/2` facts denote instead the services composing a certain version `(AppId, Version)` of the considered application.

###### Infrastructure

A Cloud-IoT infrastructure of two nodes is declared as in

```prolog
node(edge42, [(gcc,0),(caffe,4)], (6, 3), [(phone,1),(lightSensor,1)]).
node(cloud42, [(docker, 5)], (100, 1), []).

link(edge42, cloud42, 20).
```

Note that `node/4` facts denote the software, hardware and IoT capabilities of each node, associated with their estimated monthly usage cost.
Finally, `link/3` facts denote the end-to-end latency in milliseconds between two nodes.

### Osmolog at Work

Osmolog determines eligible placements (i.e. mapping from MELs to Cloud-IoT nodes) and configuration (i.e. mapping to MELs from one of their versions) of a version of an Osmotic application. Placements are ranked so to:

- minimise estimated operational costs, and
- maximise compliance to a preferred MEL version among `light`, `medium` and `full`.

A (best) candidate placement can be determined by means of an exhaustive (`exhaustive.pl`) or a heuristic search strategy (`greedy.pl`).

###### Exhaustive Search

To use the exhaustive search (`exhaustive.pl`) to determine a placement for the example application onto the example infrastructure, simply query the predicate:

```prolog
% goForBest(SortType, AppId, AppVersion, PreferredMELVersion, MaxCost, BestPlacement).
?- goForBest((0,highest), arApp, adaptive, full, 110, Best).
```

This query returns the best possible placement `BestPlacement` of the `arApp`, maximising the number of `full` MELs and minimising the operational cost, without exceeding 110 euro per month. 

The output `BestPlacement` is

```prolog
BestPlacement = [175, 75, 107, [on(usersData, full, cloud42), on(videoStorage, full, cloud42), on(movementProcessing, full, cloud42), on(arDriver, light, edge42)]] 
```

ranked 175/200, and featuring 75% version compliance and an estimated monthly cost of 107 euro.


###### Heuristic Search

To use the exhaustive search (`greedy.pl`) to determine a placement for the example application onto the example infrastructure, simply query the predicate:

```prolog
% h_placement(Application, V, PrefVersion, CapCost, HPlacement, VersionCompliance, Cost).
?- h_placement(arApp, adaptive, full, 110, HPlacement, VC, C).
```

This query returns a (sub-)optimal `HPlacement` of the `arApp`, trying to maximise the number of `full` MELs and trying to minimise the operational cost, without exceeding 110 euro per month. 

The output `HPlacement` is the same as before

```prolog
HPlacement = [on(arDriver, light, edge42), on(videoStorage, full, cloud42), on(movementProcessing, full, cloud42), on(usersData, full, cloud42)],
VC = 75,
C = 107.
```

featuring 75% version compliance and an estimated monthly cost of 107 euro. Note that, from our experiments, the heuristic search determines solutions that are on average 9% far from the optimal, achieving a 40x speed-up on execution times.
