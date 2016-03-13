---
layout: post
title:  "Explain what Erlang is about"
date:   2015-01-10 19:15:00
categories: erlang
---

These days I've been reading the second edition of Programming Erlang
by Joe Armstrong. I highly recommend it.

Looking at the book's examples I thought that it would be a good idea
to have some code to show to non Erlang programmers what Erlang is
about and why it's so fun.

Then I came up with this piece of code.

```erlang
-module(process_ex).

-export([start/0, run/1, loop/0, pid/0, stop/0]).

-define(PROCESS_NAME, proceso).

start() ->
    global:register_name(?PROCESS_NAME, spawn_link(fun() -> loop() end)).

loop()->
    receive
        Any ->
	        io:format("Received ~p ....~n", [Any]),
				        process_ex:loop()
    end.

run(N) ->
    P = pid(),

    for(1, N, fun(I) ->
                    P ! I,
                    timer:sleep(1000)
              end).

pid() ->
    global:whereis_name(?PROCESS_NAME).

stop() ->
    exit(pid(), kill).

for(N, N, F) ->
    F(N);
for(I, N, F) ->
    F(I),
    for(I + 1, N, F).
```

Why is it interesting?

# Distributed Erlang

In one terminal.

```
$ erl -sname dos@localhost
Erlang/OTP 17 [erts-6.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe]
[kernel-poll:false]

Eshell V6.1  (abort with ^G)
(dos@localhost)1> net_adm:ping(uno@localhost).
pong
(dos@localhost)2> c(process_ex).
{ok,process_ex}
(dos@localhost)3> process_ex:run(3).
3
(dos@localhost)4>
```

In another terminal.

```
$ erl -sname uno@localhost
Erlang/OTP 17 [erts-6.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe]
[kernel-poll:false]

Eshell V6.1  (abort with ^G)
(uno@localhost)1> c(process_ex).
{ok,process_ex}
(uno@localhost)2> process_ex:start().
yes
Received 1 ...
Received 2 ...
Received 3 ...
(uno@localhost)3>
```

# Globally register a name to make it independent of its pid

```erlang
start() ->
    global:register_name(?PROCESS_NAME, spawn_link(fun() -> loop() end)).

pid() ->
    global:whereis_name(?PROCESS_NAME).
```

# Make your how "for" with nice recursion

```erlang
for(N, N, F) ->
    F(N);
for(I, N, F) ->
    F(I),
    for(I + 1, N, F).
```

# Cross-VM message passing via API

```erlang
run(N) ->
    P = pid(),

    for(1, N, fun(I) -> P ! I end).
```

# Remote process control

```erlang
stop() ->
    exit(pid(), kill).
```

# Hot code reloading

In one terminal.

```
(dos@localhost)5> process_ex:run(15).
ok
(dos@localhost)6>
```

In another terminal.

```
(uno@localhost)19>
Received 1 .
Received 2 .
Received 3 .
Received 4 .
Received 5 .
Received 6 .
Received 7 .
Received 8 .
Received 9 .
Received 10 .
(uno@localhost)19> c(process_ex).
{ok,process_ex}
Received 11 .
Received 12 ....
Received 13 ....
Received 14 ....
Received 15 ....
(uno@localhost)20>
```

Compile the code and show it to your friends. No programmer can ignore
a language with so many nice features.

Have fun.
