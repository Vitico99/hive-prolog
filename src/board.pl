:- module(board, []).

:- dynamic bug/5. % bug(Player, Type of Bug, X pos, Y pos, Stack pos)
:- dynamic placeable/2. %placeable(X,Y): can place a new bug on cell X,Y
:- dynamic visited/2. % Check if cell(X,Y) has been visited by dfs

init_board():-
    assert(placeable(0,0)).

adyacent(X1,Y1,X2,Y2):- X2 is X1 - 1, Y2 is Y1.
adyacent(X1,Y1,X2,Y2):- X2 is X1 -1 , Y2 is Y1 + 1.
adyacent(X1,Y1,X2,Y2):- X2 is X1, Y2 is Y1 - 1. 
adyacent(X1,Y1,X2,Y2):- X2 is X1, Y2 is Y1 + 1. 
adyacent(X1,Y1,X2,Y2):- X2 is X1 + 1, Y2 is Y1 - 1. 
adyacent(X1,Y1,X2,Y2):- X2 is X1 + 1, Y2 is Y1.

empty(X,Y):- \+ (bug(_,_,X,Y,_)).

emptyAdyacent(X1, Y1, X2, Y2):-
    adyacent(X1, Y1, X2, Y2), empty(X2, Y2).

nonEmptyAdyacent(X,Y, X1, Y1):-
    adyacent(X,Y,X1,Y1), 
    \+empty(X1,Y1).

placeableAdyacent(X1, Y1, X2, Y2):-
    adyacent(X1, Y1, X2, Y2),
    placeable(X2, Y2).

isIsolated(X,Y):-
    findall([X1,Y1],nonEmptyAdyacent(X,Y, X1,Y1),L),
    length(L, 1).
    
isolatedEmptyAdyacent(X1, Y1, X2, Y2):- % X2, Y2 is only adyacnet to X1,Y1
    emptyAdyacent(X1,Y1,X2,Y2),
    isIsolated(X2,Y2).

placeBug(Player,Type,X,Y):-
    assert(bug(Player,Type,X,Y, 0)), retract(placeable(X,Y)),
    forall(emptyAdyacent(X,Y,X1,Y1), assert(placeable(X1, Y1))).

getAllPlaceableCells(PlaceablePositions):-
    findall([X,Y],placeable(X,Y),PlaceablePositions).


getBug(X,Y, S, bug(P,T,X,Y,S)):-%Get the bug in Position X,Y with stack number S
    bug(P,T,X,Y,S).

removeBug(X,Y):- % Remove Position X,Y. Assumes there is only one bug in cell.
    getBug(X,Y,0,Bug),
    forall(isolatedEmptyAdyacent(X,Y,X1,Y1), retract(placeable(X1,Y1))),
    retract(Bug),
    assert(placeable(X,Y)).


isBoardConnected():- \+bug(_,_,_,_,_), !.
isBoardConnected():-
    bug(_,_,X,Y,_),
    !,
    forall(visited(X1,Y1), retract(visited(X1,Y1))),
    isBoardConnected(X,Y).
isBoardConnected(X,Y):-
    assert(visited(X,Y)),
    forall(toVisit(X,Y,X2,Y2), isBoardConnected(X2,Y2)),
    findall([X3,Y3], visited(X3,Y3), AllVisited),
    findall([X4,Y4], bug(_,_,X4,Y4,0), NonEmptyCells ),
    same_length(AllVisited, NonEmptyCells).
    
toVisit(X,Y, X1,Y1):-
    nonEmptyAdyacent(X,Y,X1,Y1),
    \+visited(X1,Y1).