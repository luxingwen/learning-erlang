-module(erlcount_dispatch).

-behavior(gen_fsm).

-export([start_link/0, complete/4]).

-export([init/1, dispatching/2, listening/2, handle_event/3,
		handle_sync_event/4, handle_info/3, terminate/3, code_change/4
	]).

-define(POOL, erlcount).

-record(data, {regex = [], refs = []}).

start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

complete(Pid, Regex, Ref, Count) ->
	gen_fsm:send_all_state_event(Pid, {complete, Regex, Ref, Count}).


init([]) ->
	{ok, Re} = application:get_env(regex),
	{ok, Dir} = application:get_env(directory),
	{ok, MaxFiles} = application:get_env(max_files),
	ppool:start_pool(?POOL, MaxFiles, {erlcount_counter, start_link, []}),
	case lists:all(fun valid_regex/1, Re) of
		true ->
			self() ! {start, Dir},
			{ok, dispatching, #data{regex = [{R, 0} || R <- Re]}};
		false ->
			{stop, invalid_regex}
	end.


valid_regex(Re) ->
	try re:run("", Re) of
		_ -> true
	catch 
		error:badarg -> false
	end.


handle_info({start, Dir}, State, Data) ->
	gen_fsm:send_event(self(), erlcount_lib:find_erl(Dir)),
	{next_state, State, Data}.

dispatching({continue, File, Continuation}, Data = #data{regex = Re, refs = Refs}) ->
	F = fun({Regex, _Count}, NewRefs) ->
			Ref = make_ref(),
			ppool:async_queue(?POOL, [self(), Ref, File, Regex]),
			[Ref | NewRefs]
		end,
	NewRefs = lists:foldl(F, Refs, Re),
	gen_fsm:send_event(self(), Continuation()),
	{next_state, dispatching, Data#data{refs = NewRefs}};

dispatching(done, Data) ->
	listening(done, Data).

%% 所有的结果都收到了
listening(done, #data{regex = Re, refs = []}) ->
	[io:format("Regex ~s has ~p result~n", [R, C]) || {R, C} <- Re],
	{stop, normal, done};
%% 结果没有收集完
listening(done, Data) ->
	{next_state, listening, Data}.


handle_event({complete, Regex, Ref, Count}, State, Data = #data{regex = Re, refs = Refs}) ->
	{Regex, OldCount} = lists:keyfind(Regex, 1, Re),
	NewRe = lists:keyreplace(Regex, 1, Re, {Regex, OldCount + Count}),
	NewData = Data#data{regex = NewRe, refs = Refs -- [Ref]},
	case State of
		dispatching ->
			{next_state, dispatching, NewData};
		listening ->
			listening(done, NewData)
	end.


handle_sync_event(Event, _From, State, Data) ->
	io:format("Unexpected event: ~p~n", [Event]),
	{next_state, State, Data}.

terminate(_Reason, _State, _Data) ->
	ok.

code_change(_OldVsn, State, Data, _Extra) ->
	{ok, State, Data}.

