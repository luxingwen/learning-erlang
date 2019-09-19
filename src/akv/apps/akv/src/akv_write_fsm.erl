-module(akv_write_fsm).

-behavior(gen_fsm).

-export([start_link/7, run/7]).

-export([
		init/1,
		handle_event/3, 
		handle_info/3,
		handle_sync_event/4,
		code_change/4,
		terminate/3
	]).

-export([
		prepare/2,
		execute/2,
		waiting/2
	]).


-record(state, {
	req_id :: pos_integer(),
	from :: pid(),
	n :: pos_integer(),
	w :: pos_integer(),
	key :: term(),
	params :: term(),
	accum = [],
	action :: atom(),
	preflist :: riak_core_apl:preflist2(),
	num_w = 0 :: non_neg_integer()
	}).


start_link(ReqId, From, Key, Params, N, W, Action) ->
	gen_fsm:start_link(?MODULE, [ReqId, From, Key, Params, N, W, Action], []).


run(Action, Key, Params, N, W, Pid, ReqId) ->
	akv_write_fsm_sup:start_fsm([ReqId, Pid, Key, Params, N, W, Action]),
	{ok, ReqId}.


init([ReqId, From, Key, Params, N, W, Action]) ->
	SD = #state{req_id = ReqId, from = From, action = Action, key = Key,params = Params, n = N, w = W},
	{ok, prepare, SD, 0}.


prepare(timeout, SD0 = #state{n = N, key = Key}) ->
	DocIdx = riak_core_util:chash_key(Key),
	Preflist = riak_core_apl:get_apl(DocIdx, N, akv),
	SD = SD0#state{preflist = Preflist},
	{next_state, execute, SD, 0}.

execute(timeout, SD0 = #state{req_id = ReqId, action = Action, params = Params, preflist = Preflist}) ->
	Command = {Action, ReqId, Params},
	riak_core_vnode_master:command(Preflist, Command, {fsm, undefined, self()}, akv_vnode_master),
	{next_state, waiting, SD0}.

waiting({ReqId, Result}, SD0 = #state{from = From, num_w = NumW0, w = W, accum = Accum}) ->
	NumW = NumW0 + 1,
	Accum1 = [Result | Accum],
	SD = SD0#state{num_w = NumW, accum = Accum1},
	if 
		NumW =:= W ->
			From ! {ReqId, {ok, Accum1}},
			{stop, normal, SD};
		true ->
			{next_state, waiting, SD}
	end.

handle_info(Info, _StateName, StateData) ->
	{stop, badmsg, StateData}.

handle_event(Event, _StateName, StateData) ->
	{stop, badmsg, StateData}.

handle_sync_event(Event, _From, _StateName, StateData) ->
	{stop, badmsg, StateData}.

code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.


terminate(_Reason, _SN, _SD) ->
	ok.


