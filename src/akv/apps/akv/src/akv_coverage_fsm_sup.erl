-module(akv_coverage_fsm_sup).

-behavior(supervisor).

-export([start_link/0, start_fsm/1]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_fsm(Args) ->
	supervisor:start_child(?MODULE, Args).


init([]) ->
	CoverageFSM = {undefined, 
					{akv_coverage_fsm, start_link, []},
					temporary, 5000, worker, [akv_coverage_fsm]},
	{ok, {{simple_one_for_one, 10, 10}}, [CoverageFSM]}.