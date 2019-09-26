-module(test_mnesia).

-export([do_this_once/0]).

-record(shop, {item, quantity, cost}).

-record(cost, {name, price}).

do_this_once() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(shop, [{attributes, record_info(fields, shop)}]),
	mnesia:create_table(cost, [{attributes, record_info(fields, cost)}]),
	% mnesia:create_table(design, [{attributes, record_info(fields, design)}]),
	mnesia:stop().