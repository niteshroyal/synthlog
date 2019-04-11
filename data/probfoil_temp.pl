% Target Predicate
learn(profit/1).


% Background Facts
type('t1_r1','vanilla').
type_vanilla('t1_r1').
type('t1_r2','banana').
type_banana('t1_r2').
type('t1_r3','chocolate').
type_chocolate('t1_r3').
type('t1_r4','banana').
type_banana('t1_r4').
type('t1_r5','speculaas').
type_speculaas('t1_r5').
type('t1_r6','chocolate').
type_chocolate('t1_r6').
type('t1_r7','banana').
type_banana('t1_r7').
type('t1_r8','chocolate').
type_chocolate('t1_r8').
country('t1_r1','be').
country_be('t1_r1').
country('t1_r2','be').
country_be('t1_r2').
country('t1_r3','be').
country_be('t1_r3').
country('t1_r4','de').
country_de('t1_r4').
country('t1_r5','be').
country_be('t1_r5').
country('t1_r6','fr').
country_fr('t1_r6').
country('t1_r7','de').
country_de('t1_r7').
country('t1_r8','nl').
country_nl('t1_r8').
june('t1_r1','610').
june('t1_r2','170').
june('t1_r3','560').
june('t1_r4','610').
june('t1_r5','300').
june('t1_r6','430').
june('t1_r7','250').
june('t1_r8','210').
july('t1_r1','190').
july('t1_r2','690').
july('t1_r3','320').
july('t1_r4','640').
july('t1_r5','270').
july('t1_r6','350').
july('t1_r7','650').
july('t1_r8','280').
august('t1_r1','670').
august('t1_r2','520').
august('t1_r3','140').
august('t1_r4','320').
august('t1_r5','290').
august('t1_r6','300').
august('t1_r7','630').
august('t1_r8','270').
total('t1_r1','1470').
total('t1_r2','1380').
total('t1_r3','1020').
total('t1_r4','1570').
total('t1_r5','860').
total('t1_r6','1080').
total('t1_r7','1530').
total('t1_r8','760').
profit('t1_r1').
profit('t1_r2').
profit('t1_r3').
0::profit('t1_r4').
0::profit('t1_r5').
profit('t1_r6').
profit('t1_r7').
0::profit('t1_r8').
type('t2_r1','chocolate').
type_chocolate('t2_r1').
type('t2_r2','banana').
type_banana('t2_r2').
type('t2_r3','speculaas').
type_speculaas('t2_r3').
type('t2_r4','vanilla').
type_vanilla('t2_r4').
prodtime('t2_r1','60').
prodtime('t2_r2','40').
prodtime('t2_r3','70').
prodtime('t2_r4','40').

% Typing of Predicates
base(type(row_id, type_constant)).
base(type_vanilla(row_id)).
base(type_banana(row_id)).
base(type_chocolate(row_id)).
base(type_speculaas(row_id)).
base(country(row_id, country_constant)).
base(country_be(row_id)).
base(country_de(row_id)).
base(country_fr(row_id)).
base(country_nl(row_id)).
base(june(row_id, june_constant)).
base(july(row_id, july_constant)).
base(august(row_id, august_constant)).
base(total(row_id, total_constant)).
base(profit(row_id)).
base(prodtime(row_id, prodtime_constant)).

%Declarative Bias
mode(type(+, +)).
mode(type(-, +)).
mode(type(+, -)).
mode(type_vanilla(+)).
mode(type_banana(+)).
mode(type_chocolate(+)).
mode(type_speculaas(+)).
mode(country(+, +)).
mode(country(-, +)).
mode(country(+, -)).
mode(country_be(+)).
mode(country_de(+)).
mode(country_fr(+)).
mode(country_nl(+)).
mode(june(+, +)).
mode(june(-, +)).
mode(june(+, -)).
mode(july(+, +)).
mode(july(-, +)).
mode(july(+, -)).
mode(august(+, +)).
mode(august(-, +)).
mode(august(+, -)).
mode(total(+, +)).
mode(total(-, +)).
mode(total(+, -)).
mode(prodtime(+, +)).
mode(prodtime(-, +)).
mode(prodtime(+, -)).
