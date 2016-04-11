---
layout: post
title:  "Get and replace process state"
date:   2016-04-11 00:00:00
categories: state elixir erlang
---

I would like to document this nicety of the OTP. The state of any
process can be gotten and replaced at any time. Let's see how.

Four our test we need a gen_server.


```elixir
$ iex
Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe]
[kernel-poll:false] [dtrace]

Interactive Elixir (1.2.3) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> defmodule Test do
...(1)> use GenServer
...(1)> end
{:module, Test,
 <<70, 79, 82, 49, 0, 0, 10, 60, 66, 69, 65, 77, 69, 120, 68, 99, 0, 0, 2, 60,
 131, 104, 2, 100, 0, 14, 101, 108, 105, 120, 105, 114, 95, 100, 111, 99, 115,
 95, 118, 49, 108, 0, 0, 0, 4, 104, 2, ...>>,
 :ok}
iex(2)> GenServer.start_link Test, :my_initial_state, name: :server
{:ok, #PID<0.66.0>}
iex(3)>
```

Let's get its state.

```elixir
iex(3)> :sys.get_state(:server)
:my_initial_state
iex(4)>
```

And set it with a new value.

```elixir
iex(4)> :sys.replace_state(:server, fn(state) -> :the_new_state end)
:the_new_state
iex(5)> :sys.get_state(:server)
:the_new_state
iex(6)>
```

The whole documentation can be found [here](http://erlang.org/doc/man/sys.html).

Have fun.
