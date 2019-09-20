-module(akv_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    VMaster = { akv_vnode_master,
                  {riak_core_vnode_master, start_link, [akv_vnode]},
                  permanent, 5000, worker, [riak_core_vnode_master]},
    WriteFSMs = {akv_write_fsm_sup, 
    				{akv_write_fsm_sup, start_link, []},
    				permanent, infinity, supervisor, [akv_write_fsm_sup]},
    CoverageFSMs = {akv_coverage_fsm_sup,
    				 {akv_coverage_fsm_sup, start_link, []},
    				 permanent, infinity, supervisor, [akv_coverage_fsm_sup]},
    { ok,
        { {one_for_one, 5, 10},
          [VMaster, WriteFSMs, CoverageFSMs]}}.
