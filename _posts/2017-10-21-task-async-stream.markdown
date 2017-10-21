---
layout: post
title:  "Task async stream"
date:   2017-10-21 00:00:00
categories: elixir
---

It's been a long time since my last Elixir post. With this post I just want
to document this nice feature of the task module I've recently learnt about.

The Task.async_stream function allows to process collections in a lazy way allowing
to manage the concurrency level. As usual the spawned process gets linked
to the parent process.

The example is very simple, it just gets the list of files of a given directory.
For each file a process is spawned and the file name is registered as the
process name. This way it's easy to visualize in observer the processed file.

We take advantage of the timeout parameter of the return tuple to run the
sample every 5 seconds.

```erlang
# r Scheduler.Watcher

defmodule Scheduler.Watcher do
  use GenServer

  @timeout 5000
  @concurrency 50
  @sleep 1000

  def start_link(_) do
      GenServer.start_link(__MODULE__, [])
  end

  def init(_) do
    {:ok, [], 0}
  end

  def handle_info(info, state) do
    IO.inspect "init"

    Path.wildcard("/Users/juanmi/*")
      |> Task.async_stream(fn file ->
          Process.register(self(), String.to_atom(file))
          :timer.sleep(@sleep) end, max_concurrency: @concurrency,
          timeout: 120000)
      |> Stream.run

    IO.inspect "done"
    {:noreply, state, @timeout}
  end
end
```

That's it.

Have fun.
