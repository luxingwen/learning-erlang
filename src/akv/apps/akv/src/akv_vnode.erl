-module(akv_vnode).
-behaviour(riak_core_vnode).

-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_overload_command/3,
         handle_overload_info/2,
         handle_coverage/4,
         handle_exit/3]).

-ignore_xref([
             start_vnode/1
             ]).

-record(state, {partition, kv_state}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, KvState} = akv_kv_ets:new(#{partition => Partition}),
    {ok, #state { partition=Partition , kv_state = KvState}}.
    

%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    {reply, {pong, State#state.partition}, State};

handle_command({put, ReqId, {Bucket, Key, Value}}, _Sender, State = #state{kv_state = KvState, partition = Partition}) ->
    Location = [Partition, node()],
    {Res, KvState1} = akv_kv_ets:put(KvState, Bucket, Key, Value),
    {reply, {ReqId, {Location, Res}}, State#state{kv_state = KvState1}};


handle_command({get, ReqId, {Bucket, Key}}, _Sender, State = #state{kv_state = KvState, partition = Partition}) ->
    Location = [Partition, node()],
    {Res, KvState1} = akv_kv_ets:get(KvState, Bucket, Key),
    {reply, {ReqId, {Location, Res}}, State#state{kv_state = KvState1}};


handle_command({delete, ReqId, {Bucket, Key}}, _Sender, State = #state{kv_state = KvState, partition = Partition}) ->
    Location = [Partition, node()],
    {Res, KvState1} = akv_kv_ets:delete(KvState, Bucket, Key),
    {reply, {ReqId, {Location, Res}}, State#state{kv_state = KvState1}};


handle_command(Message, _Sender, State) ->
    lager:warning("unhandled_command ~p", [Message]),
    {noreply, State}.

handle_handoff_command(?FOLD_REQ{foldfun = FoldFun, acc0 = Acc0}, _Sender, State=#state{partition = Partition, kv_state = KvState}) ->
    lager:info("fold req ~p", [Partition]),
    KvFoldFun = fun({Key, Val}, AccIn) ->
                    lager:info("fold fun ~p: ~p", [Key, Val]),
                    FoldFun(Key, Val, AccIn)
                end,
    {AccFinal, KvState1} = akv_kv_ets:foldl(KvFoldFun, Acc0, KvState),
    {reply, AccFinal, State#state{kv_state = KvState1}};

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.


handle_handoff_data(BinData, State = #state{kv_state = KvState}) ->
    TermData = binary_to_term(BinData),
    lager:info("hand off data received ~p", [TermData]),
    {{Bucket, Key}, Value} = TermData,
    {ok, KvState1} = akv_kv_ets:put(KvState, Bucket, Key, Value),
    {reply, ok, State#state{kv_state = KvState1}}.

encode_handoff_item(Key, Value) ->
    term_to_binary({Key, Value}).

handle_overload_command(_, _, _) ->
    ok.

handle_overload_info(_, _Idx) ->
    ok.

is_empty(State = #state{kv_state = KvState, partition = Partition}) ->
    {IsEmpty, KvState1} = akv_kv_ets:is_empty(KvState),
    lager:info("is_empty ~p:~p ", [Partition, IsEmpty]),
    {IsEmpty, State#state{kv_state = KvState1}}.
    
delete(State=#state{kv_state=KvState, partition=Partition}) ->
    lager:info("delete ~p", [Partition]),
    {ok, KvState1} = akv_kv_ets:dispose(KvState),
    {ok, KvState2} = akv_kv_ets:delete(KvState1),
    {ok, State#state{kv_state=KvState2}}.


handle_coverage({keys, Bucket}, _KeySpaces, {_, RefId, _}, State = #state{kv_state = KvState}) ->
    {Keys, KvState1} = akv_kv_ets:keys(KvState, Bucket),
    {reply, {RefId, Keys}, State#state{kv_state = KvState1}};

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
