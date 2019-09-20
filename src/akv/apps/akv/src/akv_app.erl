-module(akv_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case akv_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, akv_vnode}]),
            ok = riak_core_node_watcher:service_up(akv, self()),
            ok = start_http_api(),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.


start_http_api() ->
	Dispatch = cowboy_router:compile([{'_', [{"/kv/:bucket/:key", akv_http_kv, []}]}]),
	HttpPort = application:get_env(akv, http_port, 8080),
	HttpAcceptors = application:get_env(akv, http_acceptors, 100),
	HttpMaxConnections = application:get_env(akv, http_max_connections, infinity),
	io:format("Starting HTTP API at port ~p", [HttpPort]),
	lager:info("Starting HTTP API at port ~p", [HttpPort]),
	{ok, _} = cowboy:start_clear(akv_http_listener, 
				[{port, HttpPort}, 
				{num_acceptors, HttpAcceptors},
				{max_connections, HttpMaxConnections}],
				#{env => #{dispatch => Dispatch}}),
	ok.