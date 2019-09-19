-module(akv).

-export([
         ping/0,
         get/2,
         put/3,
         delete/2,
         keys/1,
         keys/2
        ]).

-ignore_xref([
              ping/0
             ]).

-define(W, 3).
-define(N, 3).

-define(TIMEOUT, 5000).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    % argument to chash_key has to be a two item tuple, since it comes from riak
    % and the full key has a bucket, we use a contant in the bucket position
    % and a timestamp as key so we hit different vnodes on each call
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(os:timestamp())}),
    % ask for 1 vnode index to send this request to, change N to get more
    % vnodes, for example for replication
    N = 1,
    PrefList = riak_core_apl:get_primary_apl(DocIdx, N, akv),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, akv_vnode_master).


get(Bucket, Key) ->
  get(Bucket, Key, #{}).


get(Bucket, Key, Opts) ->
  K = {Bucket, Key},
  Params = K,
  run_quorum(get, K, Params, Opts).

put(Bucket, Key, Value) ->
 put(Bucket, Key, Value, #{}).

put(Bucket, Key, Value, Opts) ->
  K = {Bucket, Key},
  Params = {Bucket, Key, Value},
  run_quorum(put, K, Params, Opts).

delete(Bucket, Key) ->
  delete(Bucket, Key, #{}).

delete(Bucket, Key, Opts) ->
  K = {Bucket, Key},
  Params = K,
  run_quorum(delete, K, Params, Opts).


keys(Bucket) ->
  keys(Bucket, #{}).

keys(Bucket, Opts) ->
  Timeout = maps:get(timeout, Opts, ?TIMEOUT),
  akv_coverage_fsm:start({keys, Bucket}, Timeout).

run_quorum(Action, K, Params, Opts) ->
  N = maps:get(n, Opts, ?N),
  W = maps:get(w, Opts, ?W),
  Timeout = maps:get(timeout, Opts, ?TIMEOUT),
  ReqId = make_ref(),
  akv_write_fsm:run(Action, K, Params, N, W, self(), ReqId),
  wait_for_reqid(ReqId, Timeout).


wait_for_reqid(ReqId, Timeout) ->
  receive
    {ReqId, Val} -> Val
  after Timeout -> {error, timeout}
  end.


send_to_one(Bucket, Key, Cmd) ->
  DocIdx = riak_core_util:chash_key({Bucket, Key}),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 3, akv),
  [{IndexNode, _Type} | _] = PrefList,
  riak_core_vnode_master:sync_spawn_command(IndexNode, Cmd, akv_vnode_master).