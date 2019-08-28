%0.4::scope1:cell(1,1,3).
%0.6::scope1:cell(1,1,6).

%0.2::scope2:cell(1,1,3).

scope1:(0.4::cell(1,1,3);0.6::cell(1,1,6):-true).
scope2:cell(1,1,X) :- scope1:cell(1,1,X).
scope2:(profit :- cell(1,1,X), X > 4, \+ cell(1,1,3)).

query(scope2:_).
