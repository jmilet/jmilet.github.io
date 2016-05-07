---
layout: post
title:  "Ets counters"
date:   2016-05-07 00:00:00
categories: ets counters elixir erlang
---

Atomic counters is one of the most interesting ets' features.

As a prove of concept we're going to update a single counter from 100.000
different processes at the same time. While the counter update is progressing
a reader process will show every second how the counter evolves. As the counter
is going to be updated 100 times by each process the final counter value must
be 10.000.000, something that may not be true if the updates weren't atomic.

Let's see the code. It's quite simple.

```elixir
defmodule CountersMain do

  def update_counter(counter, key, n) when n > 0 do
      :ets.update_counter(counter, key, 1, {1, 0})
      :random.uniform(1000) |> :timer.sleep
      CountersMain.update_counter(counter, key, n - 1)
  end
  def update_counter(_, _, _), do: :ok

  def print_counter(counter, key) do
    IO.inspect(:ets.lookup(counter, key))
    :timer.sleep 1000
    CountersMain.print_counter(counter, key)
  end

end
```

`update_counter/3` receives a counter, a key and the number
of times it has to update the counter. It waits for a random period of time
up to a second before the next iteration.

`print_counter` prints the counter's value once per second.

## The test

First we need an ets table. It must be public because it's going to be
accessed from other processes.

```elixir
$ iex -S mix
Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe]
[kernel-poll:false] [dtrace]

Interactive Elixir (1.2.3) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> counter = :ets.new(:counter, [:public])
90131
iex(2)>
```

Then we start the reading processes.

```elixir
iex(2)> spawn fn -> CountersMain.print_counter(counter, "a") end
[]
#PID<0.88.0>
[]
[]
[]
[]
iex(3)>
```

The last step is to run the 100.000 processes each one updating the counter
100 times.

```elixir
iex(3)> 1..100_000 |> Enum.each(fn(_) -> spawn(fn ->
                        CountersMain.update_counter(counter, "a", 100) end) end)
[{"a", 22728}]
[{"a", 67367}]
[{"a", 133107}]
[{"a", 233586}]
[{"a", 347993}]
[{"a", 484397}]
[{"a", 653932}]
:ok
[{"a", 841631}]
[{"a", 1050413}]
[{"a", 1282222}]
[{"a", 1516658}]
[{"a", 1777595}]
[{"a", 2023975}]
[{"a", 2291715}]
[{"a", 2553481}]
[{"a", 2780287}]
[{"a", 2995063}]
[{"a", 3242007}]
[{"a", 3478347}]
[{"a", 3719031}]
[{"a", 3934812}]
[{"a", 4141703}]
[{"a", 4345601}]
[{"a", 4530392}]
[{"a", 4692367}]
[{"a", 4852481}]
[{"a", 5026014}]
[{"a", 5200049}]
[{"a", 5390763}]
[{"a", 5604310}]
[{"a", 5917687}]
[{"a", 6104787}]
[{"a", 6310394}]
[{"a", 6519919}]
[{"a", 6718212}]
[{"a", 6898009}]
[{"a", 7059706}]
[{"a", 7220957}]
[{"a", 7405098}]
[{"a", 7619440}]
[{"a", 7838246}]
[{"a", 8059324}]
[{"a", 8298621}]
[{"a", 8550425}]
[{"a", 8776205}]
[{"a", 8969585}]
[{"a", 9147427}]
[{"a", 9348748}]
[{"a", 9527935}]
[{"a", 9674082}]
[{"a", 9786465}]
[{"a", 9880464}]
[{"a", 9955397}]
[{"a", 9996282}]
[{"a", 10000000}]
[{"a", 10000000}]
[{"a", 10000000}]
[{"a", 10000000}]
[{"a", 10000000}]
iex(4)>
```

We can verify that the counter's updates are atomic, we haven't lost any value.

## Multiple counters per key

Just a final note. Each key can have multiple counters and default values.

```elixir
$ iex
Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe]
[kernel-poll:false] [dtrace]

Interactive Elixir (1.2.3) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> counter = :ets.new(:counter, [:public])
8211
iex(2)> :ets.update_counter(counter, "a", {2, 1}, {"a", 100, 200})
101
iex(3)> :ets.update_counter(counter, "a", {3, 5}, {"a", 100, 200})
205
iex(4)> :ets.lookup(counter, "a")
[{"a", 101, 205}]
iex(5)>
```

That's it.

Have fun.
