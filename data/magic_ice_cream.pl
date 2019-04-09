% Typing of predicates
base(flavour(row_id, flavour_constant)).
base(country(row_id, country_constant)).
base(june(row_id, june_constant)).
base(july(row_id, july_constant)).
base(august(row_id, august_constant)).
base(total(row_id, total_constant)).
base(prodtime(row_id, prodtime_constant)).

base(flavour_vanilla(row_id)).
base(flavour_banana(row_id)).
base(flavour_chocolate(row_id)).
base(flavour_speculaas(row_id)).
base(country_be(row_id)).
base(country_de(row_id)).
base(country_fr(row_id)).
base(country_nl(row_id)).
base(profit(row_id)).

% Declarative Bias
mode(flavour(+,+)).
mode(flavour(-,+)).
mode(flavour(+,-)).
mode(country(+,+)).
mode(country(-,+)).
mode(country(+,-)).
mode(june(+,+)).
mode(june(-,+)).
mode(june(+,-)).
mode(july(+,+)).
mode(july(-,+)).
mode(july(+,-)).
mode(august(+,+)).
mode(august(-,+)).
mode(august(+,-)).
mode(total(+,+)).
mode(total(-,+)).
mode(total(+,-)).
mode(prodtime(+,+)).
mode(prodtime(-,+)).
mode(prodtime(+,-)).

mode(flavour_vanilla(+)).
mode(flavour_banana(+)).
mode(flavour_chocolate(+)).
mode(flavour_speculaas(+)).
mode(country_be(+)).
mode(country_de(+)).
mode(country_fr(+)).
mode(country_nl(+)).

% Target Predicate
learn(profit/1).

% Target Examples
profit(r_1).
profit(r_2).
profit(r_3).
0::profit(r_4).
0::profit(r_5).

% Background Facts
flavour(r_1,vanilla).
country(r_1,be).
june(r_1,610).
july(r_1,190).
august(r_1,670).
total(r_1,1470).
flavour(r_2,banana).
country(r_2,be).
june(r_2,170).
july(r_2,690).
august(r_2,520).
total(r_2,1380).
flavour(r_3,chocolate).
country(r_3,be).
june(r_3,560).
july(r_3,320).
august(r_3,140).
total(r_3,1020).
flavour(r_4,banana).
country(r_4,de).
june(r_4,610).
july(r_4,640).
august(r_4,320).
total(r_4,1570).
flavour(r_5,speculaas).
country(r_5,be).
june(r_5,300).
july(r_5,270).
august(r_5,290).
total(r_5,860).
flavour(r_6,chocolate).
country(r_6,fr).
june(r_6,430).
july(r_6,350).
flavour(r_7,banana).
country(r_7,de).
june(r_7,250).
july(r_7,650).
flavour(r_8,chocolate).
country(r_8,nl).
june(r_8,210).
july(r_8,280).
flavour(r_9, chocolate).
prodtime(r_9, 60).
flavour(r_10, banana).
prodtime(r_10, 40).
flavour(r_11, speculaas).
prodtime(r_11, 70).
flavour(r_12, vanilla).
prodtime(r_12, 40).
flavour_vanilla(r_1).
country_be(r_1).
flavour_banana(r_2).
country_be(r_2).
flavour_chocolate(r_3).
country_be(r_3).
flavour_banana(r_4).
country_de(r_4).
flavour_speculaas(r_5).
country_be(r_5).
flavour_chocolate(r_9).
flavour_chocolate(r_6).
country_fr(r_6).
flavour_banana(r_10).
flavour_banana(r_7).
country_de(r_7).
flavour_speculaas(r_11).
flavour_chocolate(r_8).
country_nl(r_8).
flavour_vanilla(r_12).
