not(G):- call(G), !, fail.
not(_):- true.

empty(X,Y):- not(bug(_,_,X,Y,_)).

adyacent(X1, Y1, X2, Y2):-
(X2 is X1, Y2 is Y1+1);
(X2 is X1+1, Y2 is Y1+1);
(X2 is X1+1, Y2 is Y1-1);
(X2 is X1, Y2 is Y1-1);
(X2 is X1-1, Y2 is Y1-1);
(X2 is X1-1, Y2 is Y1+1).

/* X2, Y2 is an adyacent empty cell of X1, Y1 */
emptyAdyacent(X1, Y1, X2, Y2):-
    adyacent(X1, Y1, X2, Y2), empty(X2, Y2).

/* X2, Y2 is an adyacent cell occupied by an enemy of the player in X1, Y1 */
adyacentEnemy(X1, Y1, X2, Y2, P1):-
    adyacent(X1, Y1, X2, Y2), bug(P2, _, X2, Y2, _), P1 \= P2.

/* 
    Place a bug at cell X, Y and updates the playable cells.
    It assumes that cell X, Y is a valid cell to place the bug.
*/
placeBug(P,T,X,Y,S):-
    assert(bug(P,T,X,Y,S)), retract(playable(X,Y)),
    forall(emptyAdyacent(X,Y,X1,Y1), assert(playable(X1, Y1))).

/*
    Plays the bug directly from the hand of the player. 
    An if condition to distinguish the case that the first bug can be placed next to an enemy bug.
*/
playBug(P,T,X,Y):-
    (
        first(P) -> playable(X,Y), placeBug(P,T,X,Y,0), retract(first(P));
        true ->  playable(X,Y), not(adyacentEnemy(X,Y,_,_,P)), placeBug(P,T,X,Y,0)
    ).

init(P1, P2):-
    assert(playable(0,0)),
    assert(first(P1)),
    assert(first(P2)).