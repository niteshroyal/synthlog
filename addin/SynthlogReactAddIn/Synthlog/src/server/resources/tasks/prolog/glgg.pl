use_module('subtle-2.2.pl').

global_lgg(0, X) :- example(0, X). 
global_lgg(J, Z) :- J > 0, I is J-1, global_lgg(I, X), example(J,Y), lgg(X,Y,Z).

list_lgg([], []).
list_lgg([X|T], LGG) :- example(X, E), list_lgg(T, E, LGG).

list_lgg([], LGG, LGG).
list_lgg([H|T], X, LGG) :- example(H, E), lgg(X, E, NLGG), list_lgg(T, NLGG, LGG).