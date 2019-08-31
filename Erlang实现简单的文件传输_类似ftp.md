#### 不同Erlang主机节点简单的文件传输



当我们需要在两台不同的主机上进行文件传输（上传，下载），类似ftp的功能。



如果我们两台机器都装有了Erlang，那么问题就变得简单了起来。



现在我有两台机器，它分别是MacBook Pro 和一台PC，为了学习方便，它们都装上了Erlang。



它们的ip地址如图



![](img/erlang_ftp.png)





在pc启动我们的Erlang节点。(pc是windows系统，所以我们使用werl的方式启动我们的Erlang)



> werl -name pc@192.168.3.2 -setcookie asd



在MBP上启动另外一个节点。

> erl -name mbp@192.168.3.3 -setcookie asd





在mbp节点上，连接我们的pc节点。



```erlang
(mbp@192.168.3.3)1> net_kernel:connect_node('pc@192.168.3.2').
true
(mbp@192.168.3.3)2> nodes().
['pc@192.168.3.2']
```



这样就连接成功了，nodes() 会返回我们连接的节点列表。



- 查看pc上当前的工作目录。



```erlang
(mbp@192.168.3.3)3> rpc:call('pc@192.168.3.2', file, get_cwd, []).
{ok,"d:/work/erlang/study"}
```



- 列出pc当前工作目录下的文件列表

```erlang
(mbp@192.168.3.3)4> rpc:call('pc@192.168.3.2', file, list_dir, ["."]).
{ok,["bg.png","ftp","test.txt"]}
```

- 把文件下载到本地。

```erlang
(mbp@192.168.3.3)5> {ok, Bin} = rpc:call('pc@192.168.3.2', file, read_file, ["test.txt"]).
{ok,<<230,136,145,230,152,175,230,181,139,232,175,149,
      230,150,135,228,187,182>>}

(mbp@192.168.3.3)6> file:write_file("test.txt", Bin).
ok

(mbp@192.168.3.3)7> file:list_dir(".").
{ok,["words.sql","broadcast.erl","broadcast.beam",
     "test.txt"]}
```

- 把文件上传到远程节点

```erlang
(mbp@192.168.3.3)8> {ok, Bin2} = file:read_file("words.sql").
{ok,<<"/*\r\nNavicat MySQL Data Transfer\r\n\r\nSource Server         : local\r\nSource Server Version : 50718\r\nSource Host"...>>}

(mbp@192.168.3.3)9> rpc:call('pc@192.168.3.2', file, write_file, ["words.sql", Bin2]).
ok

(mbp@192.168.3.3)10> rpc:call('pc@192.168.3.2', file, list_dir, ["."]).
{ok,["bg.png","ftp","test.txt","words.sql"]}
```





这样就okok啦。