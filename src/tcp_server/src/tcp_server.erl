-module(tcp_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-behavior(gen_server).

-record(state, {socket}).

start_link(Socket) ->
	gen_server:start_link(?MODULE, [Socket], []).

init([Socket]) ->
	gen_server:cast(self(), accept),
	{ok, #state{socket = Socket}}.

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast(accept, State = #state{socket = Socket}) ->
	{ok, AcceptSocket} = gen_tcp:accept(Socket),
	tcp_server_sup:start_socket(),
	{noreply, State#state{socket = AcceptSocket}}.


handle_info({tcp, Socket, Str}, State) ->
	io:format("recive --> ~p~n", [Str]),
	gen_tcp:send(Socket, io_lib:format("server send ~p", [Str])),
	{noreply, State};

handle_info({tcp_close, _Socket, _}, State) ->
	{stop, normal, State};

handle_info({tcp_error, _Socket, _}, State) ->
	{stop, normal, State};

handle_info(_Msg, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(Reason, _State) ->
	io:format("~p  terminate reason: ~p~n", [self(), Reason]),
	ok.
