## 先安装openssl
下载openssl源码

$ wget http://www.openssl.org/source/openssl-1.0.2a.tar.gz

$ tar -zxvf openssl-1.0.2a.tar.gz

进入源码目录，注意如果不是最新下的目录，需要先执行 make clean 确保能够重新编译成功
为了不和系统的openssl冲突，我们安装的时候需要指定安装的路径

$ make clean

$ ./configure --prefix=/usr/local/opt

config之后，会生成Makefile，打开Makefile找到cc，在CFLAG参数列表里加上-fPIC

CC= cc

CFLAG= -fPIC -DOPENSSL_THREADS -D_REENTRANT -DDSO_DLFCN -DHAVE_DLFCN_H -Wa,—noexecstack -m64 -DL_ENDIAN -DTERMIO -O3 -Wall -DOPENSSL_IA32_SSE2 -DOPENSSL_BN_ASM_MONT -DOPENSSL_BN_ASM_MONT5 -DOPENSSL_BN_ASM_GF2m -DSHA1_ASM -DSHA256_ASM -DSHA512_ASM -DMD5_ASM -DAES_ASM -DVPAES_ASM -DBSAES_ASM -DWHIRLPOOL_ASM -DGHASH_ASM

编译并安装

$ sudo make && make install



## 安装Java

如果没有jdk，务必自行安装下jdk1.6 以上



## 安装erlang

官网下载最新的Erlang源码包。<https://www.erlang.org/downloads>

解压，然后进入到erlang源码目录下。

$ make clean

$ sudo ./configure  --prefix=/your/path/erlang --with-ssl=/usr/local/opt/ssl --without-javac

$ sudo make && make install 

> 如果安装过程中报错，出现依赖缺失，先安装缺失依赖，然后在依次执行以上几个命令



添加环境变量



```shell
export PATH=$PATH:/your/path/erlang/bin
```



## 安装rebar3



从官网下载 rebar3  <https://www.rebar3.org/>



赋予可执行权限



$ sudo chmod a+x rebar3



复制到shell可访问的bin目录 或者添加到环境变量。我这里选择的是 复制到 bin目录



$ sudo mv rebar3 /usr/local/bin



#### 最后

如果有什么问题，请提交issue与我交流