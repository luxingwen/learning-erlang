-module(trade_fsm).

-behavior(gen_fsm).

-export([start/1, start_link/1, trade/2, accept_trade/1,
		make_offer/2, retract_offer/2, ready/1, cancel/1
	]).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
	idle/2, idle/3, idle_wait/2, idle_wait/3, negotiate/2, negotiate/3, wait/2, ready/2, ready/3
	]).

start(Name) ->
	gen_fsm:start(?MODULE, [Name], []).


start_link(Name) ->
	gen_fsm:start_link(?MODULE, [Name], []).

%% 请求开始交易会话。 当/如果对方接受时返回。
trade(OwnPid, OtherPid) ->
	gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).


%% 接受某个玩家的交易请求
accept_trade(OwnPid) ->
	gen_fsm:sync_send_event(OwnPid, accept_negotiate).

%% 从物品表中选择一个物品进行交易
make_offer(OwnPid, Item) ->
	gen_fsm:send_event(OwnPid, {make_offer, Item}).

%% 撤销 某个交易物品
retract_offer(OwnPid, Item) ->
	gen_fsm:send_event(OwnPid, {retract_offer, Item}).

%% 宣布自己就绪。 当对方也宣布自己就绪时， 交易就完成了
ready(OwnPid) ->
	gen_fsm:sync_send_event(OwnPid, ready, infinity).

%% 取消交易
cancel(OwnPid) ->
	gen_fsm:sync_send_all_state_event(OwnPid, cancel).


%% 向另外一个fsm 发起交易会话请求
ask_negotiate(OtherPid, OwnPid) ->
	gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).


%% 转发玩家的交易接受消息
accept_negotiate(OtherPid, OwnPid) ->
	gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).


%% 转发玩家的交易物品提供消息
do_offer(OtherPid, Item) ->
	gen_fsm:send_event(OtherPid, {do_offer, Item}).

%% 转发玩家的交易物品撤销消息
undo_offer(OtherPid, Item) ->
	gen_fsm:send_event(OtherPid, {undo_offer, Item}).

%% 询问对方是否就绪
are_you_ready(OtherPid) ->
	gen_fsm:send_event(OtherPid, are_you_ready).

%% 回复未就绪
%% 也就是说，不在 wait 状态
not_yet(OtherPid) ->
	gen_fsm:send_event(OtherPid, not_yet).

%% 通知对方玩家当前处于等待进入ready状态。
%% 状态会迁移到 'ready'

am_ready(OtherPid) ->
	gen_fsm:send_event(OtherPid, 'ready!').

%% 确认fsm 处于ready 状态
ack_trans(OtherPid) ->
	gen_fsm:send_event(OtherPid, ack).

%% 询问是否可以提交
ask_commit(OtherPid) ->
	gen_fsm:sync_send_event(OtherPid, ask_commit).

%% 开始同步提交
do_commit(OtherPid) ->
	gen_fsm:sync_send_event(OtherPid, do_commit).

%% 交易取消 通知对方的fsm
notify_cancel(OtherPid) ->
	gen_fsm:send_all_state_event(OtherPid, cancel).


-record(state, {
	name = "",
	other,
	ownitems = [],
	otheritems = [],
	monitor,
	from
	}).

init(Name) ->
	{ok, idle, #state{name = Name}}.


%% 给玩家发送一条通知
notice(#state{name = N}, Str, Args) ->
	io:format("~s: "++Str++"~n", [N|Args]).

%% 记录非期望的消息
unexpected(Msg, State) ->
	io:format("~p received unknown event ~p while in state ~p~n", [self(), Msg, State]).


idle({ask_negotiate, OtherPid}, S = #state{}) ->
	Ref = monitor(process, OtherPid),
	notice(S, "~p asked for a trade negotiate", [OtherPid]),
	{next_state, idle_wait, S#state{other = OtherPid, monitor = Ref}};

idle(Event, State) ->
	unexpected(Event, idle),
	{next_state, idle, State}. 


idle({negotiate, OtherPid}, From, S = #state{}) ->
	ask_negotiate(OtherPid, self()),
	notice(S, "asking user ~p for a trade", [OtherPid]),
	Ref = monitor(process, OtherPid),
	{next_state, idle_wait, S#state{other = OtherPid, monitor = Ref, from = From}};

idle(Event, _From, State) ->
	unexpected(Event, idle),
	{next_state, idle, State}.


idle_wait({ask_negotiate, OtherPid}, S = #state{other = OtherPid}) ->
	gen_fsm:reply(S#state.from, ok),
	notice(S, "starting negotiation", []),
	{next_state, negotiate, S};

%% 对方接受了我们的请求， 迁移到negotiate 状态
idle_wait({accept_negotiate, OtherPid}, S = #state{other = OtherPid}) ->
	gen_fsm:reply(S#state.from, ok),
	notice(S, "starting negotiation", []),
	{next_state, negotiate, S};

idle_wait(Event, State) ->
	unexpected(Event, idle_wait),
	{next_state, idle_wait, State}.


idle_wait(accept_negotiate, _From, S = #state{other = OtherPid}) ->
	accept_negotiate(OtherPid, self()),
	notice(S, "accpting negotiation", []),
	{reply, ok, negotiate, S};

idle_wait(Event, _From, State) ->
	unexpected(Event, idle_wait),678
	{next_state, idle_wait, State}.

%% 向物品列表中添加一件物品
add(Item, Items) ->
	[Item | Items].

%% 从物品列表中移除 一件物品
remove(Item, Items) ->
	Items -- [Item].



