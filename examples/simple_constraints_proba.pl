1/3::myScope:cell_pred(1, 1, 1, "c1").
1/3::myScope:cell_pred(1, 1, 2, "c1").
1/3::myScope:cell_pred(1, 1, 3, "c1").

1/3::myScope:cell_pred(1, 2, 1, "c1").
1/3::myScope:cell_pred(1, 2, 2, "c1").
1/3::myScope:cell_pred(1, 2, 3, "c1").

1/4::myScope:cell_pred(1, 1, 1, "c2").
1/4::myScope:cell_pred(1, 1, 2, "c2").
1/2::myScope:cell_pred(1, 1, 3, "c2").

myScope:confidence("c1", 0.5).
myScope:confidence("c2", 0.5).

% Another possibility is to have one wieght pre prediction, and not per predictor?
weight(W, X, Y) :- myScope:confidence("c1", Conf1), myScope:cell_pred(X,Y,_,"c1"), myScope:confidence("c2", Conf2), myScope:cell_pred(X,Y,_,"c2"), select_weighted(1, [Conf1, Conf2], ["c1", "c2"], W, _).
weight(W, X, Y) :- myScope:cell_pred(X,Y,_,"c1"), \+ myScope:cell_pred(X,Y,_,"c2"), select_weighted(1, [1, 0], ["c1", "c2"], W, _).
weight(W, X, Y) :- myScope:cell_pred(X,Y,_,"c2"), \+ myScope:cell_pred(X,Y,_,"c1"), select_weighted(2, [0, 1], ["c1", "c2"], W, _).

% If both predictors have a prediction for x,y: we combine them
myScope:final_pred(X,Y,V):- weight("c1", X, Y), myScope:cell_pred(X,Y,V,"c1").
myScope:final_pred(X,Y,V):- weight("c2", X, Y), myScope:cell_pred(X,Y,V,"c2").

% If only 1 predictor has a prediction, we take its prediciton as is
%myScope:final_pred(X,Y,V):- myScope:cell_pred(X,Y,V,"c2"), \+myScope:cell_pred(X,Y,_,"c1").
%myScope:final_pred(X,Y,V):- myScope:cell_pred(X,Y,V,"c1"), \+myScope:cell_pred(X,Y,_,"c2").

% User is at 70pct sure that cell(1,1)<= 2
myScopeConstraints:(0.7::wrong :- cell_pred(1, 1, X, _), X > 2).
% User knows the cell(1,2)<=2
myScopeConstraints:(wrong :- cell_pred(1, 2, X, _), X > 2).

myScopeConstraints:X :- myScope:X.
contains_clauses(myScopeConstraints).


query(myScopeConstraints:final_pred(_,_,_)).