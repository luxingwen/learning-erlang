## 源码构建emqx以及其第三方插件



要想编译emqx以及其第三方插件，使用[emqx-rel](<https://github.com/emqx/emqx-rel>) 这个项目来构建。官方写的还是比较明白，对于没有重来都没有接触过Erlang的小伙伴来说可能会遇到一些问题。

#### 首先，源码安装最新版的Erlang和rebar3



linux 查看 [Linux源码安装Erlang-添加OpenSSL依赖,安装rebar3](Linux源码安装Erlang-添加OpenSSL依赖,安装rebar3.md)  这篇文章

mac 查看 <https://www.jianshu.com/p/efeced0cf8f7>  这篇文章



因为需要openssl 依赖，不是源码安装可能没有，到时候编译的时候可能会通过，运行时可能会报错。所以，这里还是选择源码安装Erlang，把依赖手工加上。



#### 源码构建emqx



$ git clone https://github.com/emqx/emqx-rel.git

$ cd emqx-rel 

$ make



第一次构建，rebar3 会帮我们去下载依赖。它使用的是git，所以，电脑上必须的装有 **git**，而且可以正常 clone github上的项目。



启动 emqx

$ _build/emqx/rel/emqx/bin/emqx start

停止 emqx

$ _build/emqx/rel/emqx/bin/emqx stop





#### 编译第三方插件

如果你可以源码正确构建 emqx，那么构建第三方插件 应该很简单才对。



你可以试着 根据  <https://github.com/luxingwen/emqx-mysql>  上所写的文档来构建 emqx-mysql 这个插件。



  ### 最后



我使用的是 Ubuntu。若你在构建emqx 及其 第三方插件时还有 疑问，请在issue上与我联系，交流。

