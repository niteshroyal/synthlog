:- use_module('../synthlog/bias.py').

table(1):row(vanilla, leuven, 100, 200, 200, 20000).
table(1):row(vanilla, gent,  50, 100, 200, 14000).
table(1):row(banana, leuven, 50, 50, 50, 9000).
table(1):row(chocolate, leuven, 100, 100, 100, 12000).
table(1):row(chocolate, gent, 100, 100, 100, 12000).
table(1):row(neuzekes, gent, 150, 250, 100, 40000).

table(2):row(vanilla, 40).
table(2):row(chocolate, 40).
table(2):row(banana, 60).
table(2):row(neuzekes, 80).


target(positive, table(1):row(vanilla, leuven, 100, 200, 200, 20000), 6).
relevant(positive, table(2):row(vanilla, 40), 2).

target(negative, table(1):row(chocolate, leuven, 100, 100, 100, 12000), 6).
relevant(negative, table(2):row(chocolate, 40), 2).

fkeys:X :- target(positive, Table1:Row1, _), relevant(positive, Table2:Row2, _), foreign_keys(Row1, Row2, X).
lgg:X :-
    target(positive, Table1:Row1, _), relevant(positive, Table2:Row2, _),
    target(negative, Table1:Row3, _), relevant(negative, Table2:Row4, _),
    foreign_keys(Row1, Row2, Template),
    lgg(rows(Row1, Row2), rows(Row3, Row4), Template, X).

query(fkeys:_).
query(lgg:_).

