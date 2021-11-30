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







