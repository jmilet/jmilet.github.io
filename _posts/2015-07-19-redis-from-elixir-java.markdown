---
layout: post
title:  "Redis with Elixir and Java"
date:   2015-07-19 22:30:00
categories: redis elixir java
---

I was courious about Redis so today I've decided to take a look at
it. These are my notes on the process of building the server and
accesing it from two different languages, Elixir and Java.

# Installing Redis

Installing redis is quite simple. Download the tarball from the Redis home
page, tar xvfz it, cd into the new created directory and run make. Optionally, you
can run make test.

```bash
juanmi@jmimac2 ~/redis $ tar xfz redis-3.0.3.tar.gz
juanmi@jmimac2 ~/redis $ ll
total 2664
drwxr-xr-x@ 20 juanmi  staff      680 17 jul 11:50 redis-3.0.3
-rw-r-----@  1 juanmi  staff  1360959 19 jul 19:59 redis-3.0.3.tar.gz
juanmi@jmimac2 ~/redis $ cd redis-3.0.3
juanmi@jmimac2 ~/redis/redis-3.0.3 $ make
cd src && /Applications/Xcode.app/Contents/Developer/usr/bin/make all
rm -rf redis-server redis-sentinel redis-cli redis-benchmark redis-check-dump redis-c
heck-aof *.o *.gcda *.gcno *.gcov redis.info lcov-html
(cd ../deps && /Applications/Xcode.app/Contents/Developer/usr/bin/make distclean)
(cd hiredis && /Applications/Xcode.app/Contents/Developer/usr/bin/make clean) > /dev/
null || true
(cd linenoise && /Applications/Xcode.app/Contents/Developer/usr/bin/make clean) > /de
v/null || true
(cd lua && /Applications/Xcode.app/Contents/Developer/usr/bin/make clean) > /dev/null
|| true
(cd jemalloc && [ -f Makefile ] && /Applications/Xcode.app/Contents/Developer/usr/bin
/make distclean) > /dev/null || true
(rm -f .make-*)
(rm -f .make-*)
echo STD=-std=c99 -pedantic >> .make-settings
echo WARN=-Wall -W >> .make-settings
echo OPT=-O2 >> .make-settings
echo MALLOC=libc >> .make-settings
...
...
```

# Starting the server

The server is created as ./src/redis-server.

```bash
juanmi@jmimac2 ~/redis/redis-3.0.3 $ ./src/redis-server
5420:C 19 Jul 21:08:59.373 # Warning: no config file specified, using the default
config. In order to specify a config file use ./src/redis-server /path/to/redis.conf
5420:M 19 Jul 21:08:59.375 * Increased maximum number of open files to 10032 (it was
originally set to 256).
                _._
           _.-``__ ''-._
      _.-``    `.  `_.  ''-._           Redis 3.0.3 (00000000/0) 64 bit
  .-`` .-```.  ```\/    _.,_ ''-._
 (    '      ,       .-`  | `,    )     Running in standalone mode
 |`-._`-...-` __...-.``-._|'` _.-'|     Port: 6379
 |    `-._   `._    /     _.-'    |     PID: 5420
  `-._    `-._  `-./  _.-'    _.-'
 |`-._`-._    `-.__.-'    _.-'_.-'|
 |    `-._`-._        _.-'_.-'    |           http://redis.io
  `-._    `-._`-.__.-'_.-'    _.-'
 |`-._`-._    `-.__.-'    _.-'_.-'|
 |    `-._`-._        _.-'_.-'    |
  `-._    `-._`-.__.-'_.-'    _.-'
      `-._    `-.__.-'    _.-'
          `-._        _.-'
              `-.__.-'

5420:M 19 Jul 21:08:59.381 # Server started, Redis version 3.0.3
5420:M 19 Jul 21:08:59.382 * DB loaded from disk: 0.001 seconds
5420:M 19 Jul 21:08:59.382 * The server is now ready to accept connections on
port 6379
```

Now we can connect to it from the command line.

```bash
juanmi@jmimac2 ~/redis/redis-3.0.3 $ ./src/redis-cli
127.0.0.1:6379> set hola "que tal, bien gracias"
OK
127.0.0.1:6379> get hola
"que tal, bien gracias"
127.0.0.1:6379>
```

# The Elixir client with "exredis"

For accessing our Redis server we'll use exredis. We'll include the dependency in our mix project.

```elixir
  defp deps do
    [
        {:exredis, ">= 0.2.0"}
    ]
  end
```

We'll build the project.

```bash
juanmi@jmimac2 ~/redis_elixir $ mix deps.get
Running dependency resolution
Dependency resolution completed successfully
  eredis: v1.0.8
  exredis: v0.2.0
* Getting exredis (Hex package)
Checking package (https://s3.amazonaws.com/s3.hex.pm/tarballs/exredis-0.2.0.tar)
Using locally cached package
Unpacked package tarball (/Users/juanmi/.hex/packages/exredis-0.2.0.tar)
* Getting eredis (Hex package)
Checking package (https://s3.amazonaws.com/s3.hex.pm/tarballs/eredis-1.0.8.tar)
Using locally cached package
Unpacked package tarball (/Users/juanmi/.hex/packages/eredis-1.0.8.tar)
juanmi@jmimac2 ~/redis_elixir $ mix deps.compile
==> eredis (compile)
Compiled src/eredis_sub.erl
...
...
...
Compiled lib/exredis/sub.ex
Compiled lib/exredis/api.ex
Generated exredis app
```

After the mix deps.get, we can query our server.

```bash
juanmi@jmimac2 ~/redis_elixir $ iex -S mix
Erlang/OTP 18 [erts-7.0] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe]
[kernel-poll:false]

Compiled lib/redis_elixir.ex
Generated redis_elixir app
Interactive Elixir (1.1.0-dev) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> client = Exredis.start
#PID<0.123.0>
iex(2)> client |> Exredis.Api.set "bien", "gracias"
:ok
iex(3)>
```
We can check the new key/value directly on the server.

```bash
127.0.0.1:6379> get bien
"gracias"
127.0.0.1:6379>
```


# The Java client with "Jedis"

For the Java client we'll use Jedis.

The Maven dependency is:

```xml
<dependency>
    <groupId>redis.clients</groupId>
	<artifactId>jedis</artifactId>
	<version>2.7.2</version>
	<type>jar</type>
	<scope>compile</scope>
</dependency>
```

The Java code is the Jedis home page one.

```java
package com.mycompany.app;

import redis.clients.jedis.Jedis;

/**
 * Hello world!
 *
 */
public class App
{
    public static void main( String[] args )
    {
	Jedis jedis = new Jedis("localhost");
	jedis.set("foo", "bar");
	String value = jedis.get("foo");
    }
}
```

We'll build the test jar file.

```bash
juanmi@jmimac2 ~/test_redis $ mvn package
[INFO] Scanning for projects...
[INFO]
[INFO] ------------------------------------------------------------------------
[INFO] Building test_redis 1.0-SNAPSHOT
[INFO] ------------------------------------------------------------------------
Downloading: https://repo.maven.apache.org/maven2/redis/clients/jedis/2.7.2/jedis-2.7
.2.pom
Downloaded: https://repo.maven.apache.org/maven2/redis/clients/jedis/2.7.2/jedis-2.7.
2.pom (5 KB at 3.9 KB/sec)
Downloading: https://repo.maven.apache.org/maven2/redis/clients/jedis/2.7.2/jedis-2.7
.2.jar
Downloaded: https://repo.maven.apache.org/maven2/redis/clients/jedis/2.7.2/jedis-2.7.
2.jar (315 KB at 732.5 KB/sec)
[INFO]
[INFO] --- maven-resources-plugin:2.6:resources (default-resources) @ test_redis ---
[WARNING] Using platform encoding (UTF-8 actually) to copy filtered resources, i.e. b
uild is platform dependent!
...
...
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time: 4.053 s
[INFO] Finished at: 2015-07-19T21:55:49+02:00
[INFO] Final Memory: 12M/172M
[INFO] ------------------------------------------------------------------------
juanmi@jmimac2 ~/test_redis $
```

And run it.

```java
juanmi@jmimac2 ~/test_redis $ mvn exec:java -Dexec.mainClass="com.mycompany.app.App"
[INFO] Scanning for projects...
[INFO]
[INFO] ------------------------------------------------------------------------
[INFO] Building test_redis 1.0-SNAPSHOT
[INFO] ------------------------------------------------------------------------
[INFO]
[INFO] --- exec-maven-plugin:1.4.0:java (default-cli) @ test_redis ---
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time: 1.088 s
[INFO] Finished at: 2015-07-19T21:58:22+02:00
[INFO] Final Memory: 9M/155M
[INFO] ------------------------------------------------------------------------
juanmi@jmimac2 ~/test_redis $
```

The new key/value is in the server.

```bash
127.0.0.1:6379> get foo
"bar"
127.0.0.1:6379>
```

That's it.

Have fun.
