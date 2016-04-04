---
layout: post
title:  "Limited parallel map"
date:   2016-04-04 00:00:00
categories: processes elixir erlang
---

I just wanted to document this function. It's a parallel map that limits
the number of concurrent processes.

It's quite simple, it plays with recursion and starts a new process
for each element of a list. When the limit of processes is reached it waits
for a response before to start another a new process. This way we can easily
control the load.

A basic test to prove that everything works.

```elixir
defmodule ParallelTest do
  use ExUnit.Case
  doctest Parallel

  test "basic run" do
    number = 4

    res = 1..number
      |> Enum.to_list
      |> Parallel.run(2, fn(x) ->
                            :erlang.timestamp |> :random.seed
                            :random.uniform(1000) |> :timer.sleep
                            x * 2
                         end)
      |> Enum.sort
      |> IO.inspect

    assert res == 1..number |> Enum.map(fn(x) -> {x, x * 2} end)
  end
end
```

The code.

```elixir
defmodule Parallel do

  # --------------------- Api ---------------------
  def run(col, workers, fun) do
    run(col, fun, workers, 0, [])
  end

  # --------------------- Priv ---------------------
  defp run([h|t], fun, workers, n, res) when n < workers do
    me = self()

    IO.puts "total: #{n + 1}"

    spawn_link fn ->
      send me, {h, fun.(h)}
    end
    run(t, fun, workers, n + 1, res)
  end

  defp run([], fun, workers, n, res) when n > 0 do
    receive do
      value -> run([], fun, workers, n - 1, [value|res])
    end
  end

  defp run([], _fun, _workers, _n, res) do
    res
  end

  defp run(col, fun, workers, n, res) do
    receive do
      value -> run(col, fun, workers, n - 1, [value|res])
    end
  end

  # --------------------- Helper ---------------------
  def run_helper(number, workers) do
    1..number
      |> Enum.to_list
      |> Parallel.run(workers, fn(x) ->
                                 :erlang.timestamp |> :random.seed
                                 :random.uniform(1000) |> :timer.sleep
                                 x * 2
                               end)
  end
end
```

Let's run the test.

```bash
$ mix test
Compiled lib/parallel.ex
total: 1
total: 2
total: 2
total: 2
[{1, 2}, {2, 4}, {3, 6}, {4, 8}]
.

Finished in 1.3 seconds (0.05s on load, 1.3s on tests)
1 test, 0 failures

Randomized with seed 681462
$
```

Some iex testing.

```bash
$ iex -S mix
Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe]
[kernel-poll:false] [dtrace]

Interactive Elixir (1.2.3) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> Parallel.run_helper(10, 2) |> Enum.sort
total: 1
total: 2
total: 2
total: 2
total: 2
total: 2
total: 2
total: 2
total: 2
total: 2
[{1, 2}, {2, 4}, {3, 6}, {4, 8}, {5, 10}, {6, 12}, {7, 14}, {8, 16}, {9, 18},
 {10, 20}]
iex(2)> Parallel.run_helper(10, 7) |> Enum.sort
total: 1
total: 2
total: 3
total: 4
total: 5
total: 6
total: 7
total: 7
total: 7
total: 7
[{1, 2}, {2, 4}, {3, 6}, {4, 8}, {5, 10}, {6, 12}, {7, 14}, {8, 16}, {9, 18},
 {10, 20}]
iex(3)> Parallel.run_helper(5, 1) |> Enum.sort
total: 1
total: 1
total: 1
total: 1
total: 1
[{1, 2}, {2, 4}, {3, 6}, {4, 8}, {5, 10}]
iex(4)>
```

That's it.

Corrections and improvements are welcome.

Have fun.
