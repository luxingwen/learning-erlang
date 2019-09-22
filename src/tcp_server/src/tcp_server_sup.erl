-module(tcp_server_sup).

-behavior(supervisor).

-export([start_link/0, start_socket/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, Port} = application:get_env(tcp_server_app, port),
	{ok, ListenSocket} = gen_tcp:listen(Port, [{active, true}, {packet, line}]),
	spawn_link(fun empty_listeners/0),

	{ok,{{simple_one_for_one, 60, 3600},
		[{tcpserver, {tcp_server, start_link, [ListenSocket]},
		temporary, 1000, worker, [tcp_server]}]
	}}.

start_socket() ->
	supervisor:start_child(?MODULE, []).

empty_listeners() ->
	[start_socket() || _ <- lists:seq(1, 2)].