### Ekka源码阅读



Ekka是emqx自动集群和自动愈合的一个组件。



#### 节点发现与自动集群



**ekka_cluster_strategy** 模块定义了一些行为。它们分别是：



```erlang
-type(options() :: list(proplists:property())).

%% 发现
-callback(discover(options()) -> {ok, list(node())} | {error, term()}).

%% 锁
-callback(lock(options()) -> ok | ignore | {error, term()}).

%% 解锁
-callback(unlock(options()) -> ok | ignore | {error, term()}).

%% 注册
-callback(register(options()) -> ok | ignore | {error, term()}).

%% 移除注册
-callback(unregister(options()) -> ok | ignore | {error, term()}).
```



**ekka_cluster**模块提供一些集群的API调用和集群管理的RPC调用。它们分别是

```erlang
%% Cluster API
-export([
	join/1,					%% 加入
	leave/0, 				%% 离开
	force_leave/1, 			%% 强制离开
	status/0 				%% 状态
	]).


%% RPC Call for Cluster Management
-export([
	prepare/1,
	heal/1,
	reboot/0
	]).


%% @doc Join the cluster
%% 加入集群
-spec(join(node()) -> ok | ignore | {error, any()}).

%% 如果节点是当前节点，则忽略
join(Node) when Node =:= node() ->
    ignore;
join(Node) when is_atom(Node) ->
    case {ekka_mnesia:is_node_in_cluster(Node), ekka_node:is_running(Node, ekka)} of
        {false, true} -> 
        	%% 如果节点没有在集群里而且节点正在运行， 则加入这个集群
            prepare(join), ok = ekka_mnesia:join_cluster(Node), reboot();
        {false, false} ->
        	%% 如果节点没有在集群里而且节点也没有运行  返回 错误
            {error, {node_down, Node}};
        {true, _} ->
        	%% 如果节点已经在集群里运行
            {error, {already_in_cluster, Node}}
    end.

%% @doc Leave from the cluster.
%% 离开集群
-spec(leave() -> ok | {error, any()}).
leave() ->
    case ekka_mnesia:running_nodes() -- [node()] of
        [_|_] ->
            %% 如果该节点在运行的节点列表里  离开该集群
            prepare(leave), ok = ekka_mnesia:leave_cluster(), reboot();
        [] ->
            {error, node_not_in_cluster}
    end.


%% @doc Force a node leave from cluster.
%% 强制一个节点离开集群
-spec(force_leave(node()) -> ok | ignore | {error, term()}).

%% 如果是当前节点，忽略
force_leave(Node) when Node =:= node() ->
    ignore;
force_leave(Node) ->
    %% 如果Node节点在集群里，接着rpcdia
    case ekka_mnesia:is_node_in_cluster(Node)
         andalso rpc:call(Node, ?MODULE, prepare, [leave]) of
        ok ->
            case ekka_mnesia:remove_from_cluster(Node) of
                ok    -> rpc:call(Node, ?MODULE, reboot, []);
                Error -> Error
            end;
        false ->
            {error, node_not_in_cluster};
        {badrpc, nodedown} ->
            ekka_membership:announce({force_leave, Node}),
            ekka_mnesia:remove_from_cluster(Node);
        {badrpc, Reason} ->
            {error, Reason}
    end.


%% @doc Cluster status.
%% 集群状态
status() -> ekka_mnesia:cluster_status().

```





**ekka_autocluster** 自动集群模块



```erlang
-spec(run(atom()) -> any()).
run(App) ->
    %% 获得锁
    case acquire_lock(App) of
        ok ->
            spawn(fun() ->
            		 %% 把当前进程的组长设置为init
                      group_leader(whereis(init), self()),
                      %% 等待应用准备就绪
            		  wait_application_ready(App, 10),
                      try
                          %% 发现且加入
                          discover_and_join()
                      catch
                          _:Error:Stacktrace ->
                              ?LOG(error, "Discover error: ~p~n~p", [Error, Stacktrace])
                      after
                          %% 释放锁
                          release_lock(App)
                      end,
            		  %% 可能需要再次运行
                      maybe_run_again(App)
                  end);
        failed -> ignore
    end.

%% 等待节点运行应用
wait_application_ready(_App, 0) ->
    timeout;
wait_application_ready(App, Retries) ->
    case ekka_node:is_running(App) of
        true  -> ok;
        false -> timer:sleep(1000),
                 wait_application_ready(App, Retries - 1)
    end.

%% 可能需要在尝试一次
maybe_run_again(App) ->
    %% Check if the node joined cluster?
    %% 检查节点是否在集群里
    case ekka_mnesia:is_node_in_cluster() of
        true  -> ok;
        false -> 
            	 %% 如果节点没有加入集群， 5秒后再次重试
            	 timer:sleep(5000),
                 run(App)
    end.

-spec(discover_and_join() -> any()).
discover_and_join() ->
    with_strategy(
      fun(Mod, Options) ->
        case Mod:lock(Options) of
            ok ->
                discover_and_join(Mod, Options),
                log_error("Unlock", Mod:unlock(Options));
            ignore ->
                timer:sleep(rand:uniform(3000)),
                discover_and_join(Mod, Options);
            {error, Reason} ->
                ?LOG(error, "AutoCluster stopped for lock error: ~p", [Reason])
        end
      end).


-spec(acquire_lock(atom()) -> ok | failed).
%% 获取锁
acquire_lock(App) ->
    %% 如果应用程序APP的配置参数 autocluster_lock没有被设置值，则设置为true，表示获得锁成功，否则获取锁失败
    case application:get_env(App, autocluster_lock) of
        undefined ->
            application:set_env(App, autocluster_lock, true);
        {ok, _} -> failed
    end.

-spec(release_lock(atom()) -> ok).
%% 释放锁
release_lock(App) ->
    %% 清除应用程序APP的配置参数 autocluster_lock
    application:unset_env(App, autocluster_lock).
```

