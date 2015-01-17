---
layout: post
title:  "Links and Monitors"
date:   2015-01-17 19:00:00
categories: erlang
---

Maybe due to the help of OTP supervisors I have never spent too much time
getting into the inner parts of monitors and links.

Today I've decided to learn a little bit about how they work and I've
written some notes down. They don't try to be a detailed explanation,
there are hundreds of better resources for that.  The intention is to
be a starting point for beginners and something I can look at when
needed.

I hope you find them useful, but as I always say, be careful, I'm learning
and I might be wrong... In that case I'd love to be corrected.

# The point of links and monitors

The idea is pretty simple. Two or more processes establish a dependent
relationship between them in order to manage expected or unexpected
process exits.

For example, we can have two processes that only make sense if both of
them are running. We don't want any of them running alone if the other
doesn't exist.

```
A --------> B    Monitor
A <-------  B    Monitor
A <-------> B    Link
```

Links and monitors are the mechanism that allows this management. They
differ in how it is acomplished.

# Links are bidirectional, mono-instance, you must to ask for notifications and they only react on non normal exits

1. Bidirectional

	If processes A and B are linked and process A abnormally exits then B exits
    (and the other way around).

2. Mono-instance

	It doesn't matter how many links you make between two processes,
	they are linked by just one link.

3. We must ask for notification if we don't want all the linked processes to exit together

	By default, when two processes are linked, if one of them abnormally exits
	the other one also does. This also applies to several linked processes.

	You can modify this behaviour setting a process's flag which goal
	is to intercept exits and transform them into regular Erlang messages
	with this structure:

		{'EXIT', From, Reason}

	It's important to note that this interception breaks the exit progration chain. In other words,
	when and exit is traped, the exit propagation ends.

4. They only react on non normal exits

	Note how the shell's pid evolves.

	```Erlang
	$ erl
	Erlang/OTP 17 [erts-6.1] [source] [64-bit] [smp:4:4] [async-threads:10]
	[hipe] [kernel-poll:false]

	Eshell V6.1  (abort with ^G)
	1> self().
	<0.32.0>
	2> spawn_link(fun() -> receive after 200 -> exit(normal) end end).
	<0.35.0>
	3> self().
	<0.32.0>
	4> spawn_link(fun() -> receive after 200 -> exit(error) end end).
	<0.38.0>
	** exception error: error
	5> self().
	<0.40.0>
	6>
	```

<br>

# Monitors are unidirectional, stackable, we don't need to ask for notifications and they react on normal and non normal exits

1. Unidirectional

	One process monitors another process, but not the other way around.

2. Monitors are stackable

	Each monitor has a unique reference in the system, and we can have many of them.

	```Erlang
	$ erl
	Erlang/OTP 17 [erts-6.1] [source] [64-bit] [smp:4:4] [async-threads:10]
	[hipe] [kernel-poll:false]

	Eshell V6.1  (abort with ^G)
	1> P = spawn(fun() -> receive after 20000 -> ok end end).
	<0.34.0>
	2> monitor(process, P).
	#Ref<0.0.0.34>
	3> monitor(process, P).
	#Ref<0.0.0.39>
	4> monitor(process, P).
	#Ref<0.0.0.44>
	5> flush().
	Shell got {'DOWN',#Ref<0.0.0.34>,process,<0.34.0>,normal}
	Shell got {'DOWN',#Ref<0.0.0.44>,process,<0.34.0>,normal}
	Shell got {'DOWN',#Ref<0.0.0.39>,process,<0.34.0>,normal}
	ok
	6>
	```

3. We don't need to ask for notifications

	Unlike links, monitors receive regular Erlang messages by default. The structure is:

		{'DOWN', Ref, process, Pid, Reason}

4. They react on normal and non normal exits

	Note that the shell's pid never changes, this implies that the monitored process
	never goes down.

	```Erlang
	$ erl
	Erlang/OTP 17 [erts-6.1] [source] [64-bit] [smp:4:4] [async-threads:10]
	[hipe] [kernel-poll:false]

	Eshell V6.1  (abort with ^G)
	1> self().
	<0.32.0>
	2> spawn_monitor(fun() -> receive after 200 -> exit(normal) end end).
	{<0.35.0>,#Ref<0.0.0.34>}
	3> flush().
	Shell got {'DOWN',#Ref<0.0.0.34>,process,<0.35.0>,normal}
	ok
	4> self().
	<0.32.0>
	5> spawn_monitor(fun() -> receive after 200 -> error(error) end end).
	{<0.39.0>,#Ref<0.0.0.49>}
	6>
	=ERROR REPORT==== 17-Jan-2015::22:14:34 ===
	Error in process <0.39.0> with exit value: {error,[{erlang,apply,2,[]}]}


	6> self().
	<0.32.0>
	7> flush().
	Shell got {'DOWN',#Ref<0.0.0.49>,process,<0.39.0>,
    {error,[{erlang,apply,2,[]}]}}
	ok
	8>
	```

<br>

# The Erlang functions that manage links and monitors are:

**For links**

* link/1
* spawn_link/1
* spawn_link/2
* spawn_link/3
* spawn_link/4
* unlink/1

**For monitors**

* monitor/2
* spawn_monitor/1
* spawn_monitor/3
* demonitor/1
* demonitor/2
<br>
<br>

# Two examples

Let's see two simple proves of concept, one about links and another one about monitors.

The examples are just a very simple supervisor which re-starts a
simple process which exits every five seconds.


# The link example

In the link example the points to note are:

* We have to _trap exits_ in both processes in order to receive EXIT messages.
* The structure of the received message is different from the monitor one.
* Abnormally exiting the monitoring process also exits the monitored one (look for "simple_process got 'EXIT'"). This demonstrates the bidirectionality.
<br>
<br>

```Erlang
-module(simple_sup_link).
-export([start/0, loop/0]).



start() ->
    spawn(fun() -> main() end).

main() ->
    process_flag(trap_exit, true),
    spawn_link(fun() -> simple_process(5) end),
    loop().

loop() ->
    receive
	crash ->
	    1/0;
	{'EXIT', From, Reason} ->
	    io:format("loop got 'EXIT' ~p ~p~n", [From, Reason]),
	    spawn_link(fun() ->
			       process_flag(trap_exit, true),
			       simple_process(5)
		       end),
	    ?MODULE:loop()
    end.

simple_process(Seconds) ->
    io:format("Simple process running...~p~n", [self()]),

    receive
	{'EXIT', From, Reason} ->
	    io:format("simple_process got 'EXIT' ~p ~p~n", [From, Reason])
    after Seconds * 1000 ->
	    ok
    end.
```

It results in...

```Erlang
$ erl
Erlang/OTP 17 [erts-6.1] [source] [64-bit] [smp:4:4] [async-threads:10]
[hipe] [kernel-poll:false]

Eshell V6.1  (abort with ^G)
1> c(simple_sup_link).
simple_sup_link.erl:17: Warning: this expression will fail with a 'badarith' exception
{ok,simple_sup_link}
2> P = simple_sup_link:start().
Simple process running...<0.40.0>
<0.39.0>
loop got 'EXIT' <0.40.0> normal
Simple process running...<0.42.0>
loop got 'EXIT' <0.42.0> normal
Simple process running...<0.43.0>
loop got 'EXIT' <0.43.0> normal
Simple process running...<0.44.0>
3> P ! crash.
simple_process got 'EXIT' <0.39.0> {badarith,
                                    [{simple_sup_link,loop,0,
                                      [{file,"simple_sup_link.erl"},
                                       {line,17}]}]}
crash
4>
=ERROR REPORT==== 17-Jan-2015::20:35:11 ===
Error in process <0.39.0> with exit value: {badarith,[{simple_sup_link,loop,
0,[{file,"simple_sup_link.erl"},{line,17}]}]}
```
<br>
# The monitor example

In the monitor example the points to note are:

* We display the message we have receive about the monitored process ending.
* We don't need to do anything in order to get those messages.
<br>
<br>

```Erlang
-module(simple_sup_monitor).
-export([start/0, loop/0]).



start() ->
    spawn(fun() -> main() end).

main() ->
    spawn_monitor(fun() -> simple_process(5) end),
    loop().

loop() ->
    receive
	{'DOWN', _Ref, process, Pid, _Reason} ->
	    io:format("~p down...~n", [Pid]),
	    spawn_monitor(fun() -> simple_process(5) end),
	    ?MODULE:loop()
    end.

simple_process(Seconds) ->
    io:format("Simple process running...~p~n", [self()]),

    receive
    after Seconds * 1000 ->
	    ok
    end.
```

It results in...

```Erlang
$ erl
Erlang/OTP 17 [erts-6.1] [source] [64-bit] [smp:4:4] [async-threads:10]
[hipe] [kernel-poll:false]

Eshell V6.1  (abort with ^G)
1> c(simple_sup_monitor).
{ok,simple_sup_monitor}
2> simple_sup_monitor:start().
Simple process running...<0.40.0>
<0.39.0>
<0.40.0> down...
Simple process running...<0.42.0>
<0.42.0> down...
Simple process running...<0.43.0>
<0.43.0> down...
Simple process running...<0.44.0>
3>
```

That's it.

Have fun.

