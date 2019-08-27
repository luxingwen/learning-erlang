### 分布式应用



在多个Erlang节点的分布式系统中，如果正在运行的某个应用程序节点发生故障，则另外一个节点重新启动该应用程序。



#### 指定分布式应用



通过以下配置参数来指定分布式应用程序。

distributed = [{Application，Timeout, NodeDesc}]

Application = atom()  指定应用程序

Timeout = integer()  指定另一个节点重新启动之前等待的毫秒数。默认0。

NodeDesc = [Node1 | {Node2, Node3}]  是优先级顺序的节点名称列表。



为了使分布式程序正常工作， 可以运行的节点必须相互联系并协商启动应用程序的配置。使用一下的配置参数来完成。



- sync_nodes_mandatory = [Node] - 指定必须启动的其他节点（在sync_nodes_timeout指定的超时时间内 ）。
- sync_nodes_optional = [Node] - 指定可以启动的其他节点（在sync_nodes_timeout指定的超时时间内 ）。
- sync_nodes_timeout =integer（）| infinity - 指定等待其他节点启动的毫秒数。



eg:

myapp这个应用程序运行在节点 node1@lxw,如果这个节点崩溃。myapp将在节点node2@lxw 或者 node3@lxw 重新启动。这个node1@lxwde 配置文件 node1.config如下。  

```erlang
[{kernel,
  [{distributed, [{myapp, 5000, [node1@lxw, {node2@lxw, node3@lxw}]}]},
   {sync_nodes_mandatory, [node2@lxw, node3@lxw]},
   {sync_nodes_timeout, 5000}
  ]
 }
].
```

对于node2@lxw和node3@lxw的配置文件，除了sync_nodes_mandatory的节点列表不同，其它都是相同的。node2@lxw的是[node1@lxw, node3@lxw], node3@lxw的是[node1@lxw, node2@lxw]。



sync_nodes_mandatory 元组要结合sync_nodes_timeout一起使用。当用这个配置启动分布式节点时，节点会一直处于锁定状态，直到所有节点都启动并被锁定，接着，它们之间会进行同步，然后在继续运行。如果启动的所有节点时间超过了 sync_nodes_timeout 配置的时间，那么它们都将崩溃。



#### 启动和停止分布式应用程序



当所有的节点都已经启动时，通过在所有节点上调用 application:start(Application) 来启动分布式应用程序。



也可以使用启动脚本自动启动应用程序。 



eg：

```erlang
erl -sname node1 -config node1
erl -sname node2 -config node2
erl -sname node3 -config node3
```



加入启动 application:start(Application) 参数



```erlang
erl -sname node1 -config node1 -eval "application:start(Application)"
erl -sname node2 -config node2 -eval "application:start(Application)"
erl -sname node3 -config node3 -eval "application:start(Application)"
```



同样，需要在所有相关节点上调用 applicaiont:stop(Application) 来停止应用程序。



#### 故障转移



如果正在运行应用程序的节点发生故障，则应用程序将在分布式配置参数中的节点列表中列出的第一个操作节点上重新启动（在指定的超时时间之后）。这称为 **故障转移**。



应用程序在新节点上以正常方式启动，调用：

> Module:start(normal, StartArgs)



#### 恢复

如果启动节点，这个节点在分布式应用程序中有更高的运行优先权，这个应用程序将在新的节点启动，在老的节点停止，这个称为恢复。



应用程序启动通过调用：

> Module:start({takeover, Node}, StartArgs)



