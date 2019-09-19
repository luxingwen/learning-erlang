-module(akv_kv_ets).


-export([
	new/1,
	put/4,
	get/3,
	delete/3,
	delete/1,
	is_empty/1,
	dispose/1,
	foldl/3,
	keys/2
	]).

-record(state, {table_id}).


new(_Opts) ->
	TableId = ets:new(?MODULE, [set, {write_concurrency, false}, {read_concurrency, false}]),
	{ok, #state{table_id = TableId}}.


put(State = #state{table_id = TableId}, Bucket, Key, Value) ->
	K = {Bucket, Key},
	true = ets:insert(TableId, {K, Value}),
	{ok, State}.


get(State = #state{table_id = TableId}, Bucket, Key) ->
	K = {Bucket, Key},
	Res = case ets:lookup(TableId, K) of
				[] -> {not_found, K};
				[{_, Value}] -> {found, {K, Value}}
		end,
	{Res, State}.


delete(State = #state{table_id = TableId}, Bucket, Key) ->
	K = {Bucket, Key},
	true = ets:delete(TableId, K),
	{ok, State}.


keys(State = #state{table_id = TableId}, Bucket) ->
	Keys0 = ets:match(TableId, {{Bucket, '$1'}, '_'}),
	Keys = lists:map(fun first/1, Keys0),
	{Keys, State}.


is_empty(State = #state{table_id = TableId}) ->
	IsEmpty = (ets:first(TableId) =:= '$end_of_table'),
	{IsEmpty, State}.

dispose(State = #state{table_id = TableId}) ->
	true = ets:delete_all_objects(TableId),
	{ok, State}.

delete(State = #state{table_id = TableId}) ->
	true = ets:delete(TableId),
	{ok, State}.

foldl(Fun, Acc0, State = #state{table_id = TableId}) ->
	AccOut = ets:foldl(Fun, Acc0, TableId),
	{AccOut, State}.


first([V | _]) -> V.
