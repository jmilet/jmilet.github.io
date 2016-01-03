---
layout: post
title:  "Parallel file processing"
date:   2016-01-03 00:00:00
categories: parallel processing files elixir erlang
---

It's quite usual to have to process a file whose lines's processing are completely
independent to each other. In those cases each line can be processed in parallel.

There're several approaches to solve this problem. One is to logically divide the file
in parts and process them in different instances of a process. Another one is to use
a pool of workers and ask for a new process for each line, if all the pool's processes
are in use the pool blocks the main loop until a new worker is available.

Both solutions have problems. The first one can produce an unbalanced load while
the second one forces us to deal with timeouts when fetching workers. This is
something especially hard to deal with because each line could require different
processing time.

This prove of concept tries to face this problem in a different way taking
advantage of Erlang's monitors. The idea is to spawn worker processes
up to a limit and then wait for the workers termination in order to spawn new ones.
In this model success and errors are managed via callback functions.

```
                                                Worker
               +---> Producer loop process ---> Worker ---+
               |                                Worker    | -> logic callback ()
Main Process --+                                          | -> error callback ()
     |                                                    | -> last worker ended
     +---> Result loop (regular fun) <--------------------+    callback ()
                                                               (called just once)
```

We'll write a function which receives the following parameters:

1. An open file to process
2. The max number of parallel workers
3. An Erlang ref to check for our results
4. The business logic callback for each line
5. The end callback executed when the last process has finished
6. The error callback executed for each process ended with errors

<space>
# The test

The test is quite simple. The input is a text file with some data.

```bash
$ cat k.txt
1
2
3
4
0
6
7
8
9
10
$
```

The business logic is just divide 10 by the number in the current line. As
one of the file's values is zero it'll produce an error so we'll test our error callback.

`process_file(file, max_process, ref, func, end_callback, error_callback)`
is the main processing function, while the `processes_results(ref)` is
an example of how to process the result, in this case in parallel while the
results are being produced.

The code is quite simple, it's just worth mentioning that we keep spawing
processes until the max processes limit is reached, after that we wait for
the next DOWN message in order to start the next worker. If an error is
caught the error callback is called and when the last worker has finished
the end callback is executed.

The final part is just cleaning up the final DOWN messages.

So let's see the code.

```Elixir
defmodule FileParallel do

  # Start processing.
  def process_file(file, max_process, ref, func, end_callback, error_callback) do
    process_list(file, IO.read(file, :line), 0, max_process, ref, func, end_callback,
                 error_callback)
  end

  # Process loop.
  def process_list(_file, :eof, num_process, _max_process, ref, _func, end_callback,
                   _error_callback) do
    clean_downs(num_process)
    end_callback.(ref)
    :ok
  end

  def process_list(file, value, num_process, max_process, ref, func, end_callback,
                   error_callback) do
    value = value |> String.replace("\n", "")

    if num_process < max_process do
      num_process = num_process + 1
      spawn_process(value, ref, func, end_callback, error_callback)
    else
      # As previous processes are finishing new ones are spawned.
      IO.puts "<-- Element #{inspect value} is waiting for a process..."
      receive do
        {:DOWN, _ref, :process, _pid, _reason} ->
          spawn_process(value, ref, func, end_callback, error_callback)
      end
    end

    process_list(file, IO.read(file, :line), num_process, max_process, ref, func,
                 end_callback, error_callback)
  end

  # Process results.
  def processes_results(ref) do
    receive do
      {:ok, ^ref, elem, res} ->
        IO.puts "--> #{elem}: #{res}"
        processes_results(ref)
      {:error, ^ref, elem, reason} ->
        IO.puts "--> #{elem}: #{inspect reason}"
        processes_results(ref)
      {:end, ^ref} ->
        :end
    end
  end

  ###################################################################################
  # Private functions
  ###################################################################################

  # Final DOWN messages must to be cleaned.
  defp clean_downs(num_process) do
    1..num_process |> Enum.each(fn(_) ->
      receive do
        {:DOWN, _, _, _, _} -> :ok
      end
    end)
  end

  # Spawns a new process.
  defp spawn_process(elem, ref, func, end_callback, error_callback) do
    Process.monitor(spawn fn ->
      try do
        func.(elem, ref)
      rescue
        _error ->
          error_callback.(elem, ref, System.stacktrace)
      end
    end)
  end
end
```

And the test.

```Elixir
iex(1)> {:ok, file} = File.open "k.txt"
{:ok, #PID<0.100.0>}
iex(2)> shell = self()
#PID<0.98.0>
iex(3)> ref = make_ref()
#Reference<0.0.1.316>
iex(4)> spawn fn ->
...(4)>   FileParallel.process_file(file, 5, ref,
...(4)>     fn(elem, ref) ->
...(4)>       :timer.sleep(5000)
...(4)>       send shell, {:ok, ref, elem, 10 / String.to_integer(elem)}
...(4)>     end,
...(4)>     fn(ref) ->
...(4)>       send shell, {:end, ref}
...(4)>     end,
...(4)>     fn(elem, ref, reason) ->
...(4)>       send shell, {:error, ref, elem, reason}
...(4)>     end)
...(4)> end
#PID<0.116.0>
iex(5)> FileParallel.processes_results(ref)
<-- Element "6" is waiting for a process...
<-- Element "7" is waiting for a process...
<-- Element "8" is waiting for a process...
<-- Element "9" is waiting for a process...
<-- Element "10" is waiting for a process...
--> 1: 10.0
--> 2: 5.0
--> 3: 3.3333333333333335
--> 4: 2.5
--> 0: [{:erlang, :/, [10, 0], []}, {:erl_eval, :do_apply, 6, [file: 'erl_eval.erl',
         line: 669]}, {:erl_eval, :expr_list, 6, [file: 'erl_eval.erl', line: 877]},
         {:erl_eval, :expr, 5, [file: 'erl_eval.erl', line: 236]}, {:erl_eval,
         :expr_list, 6, [file: 'erl_eval.erl', line: 877]}, {:erl_eval, :expr, 5,
         [file: 'erl_eval.erl', line: 404]}, {FileParallel,
         :"-spawn_process/5-fun-0-", 4, [file: 'lib/file_parallel.ex', line: 64]}]
--> 6: 1.6666666666666667
--> 7: 1.4285714285714286
--> 8: 1.25
--> 9: 1.1111111111111112
--> 10: 1.0
:end
iex(6)>
```

Some improvements would be needed like timeouts in the receive clauses, but I'd
rather keep the code simple for the post. Also, a distributed execution could
be possible just passing a node name to :erlang.spawn in the `spawn_process`
function.

This just a learning exercise, so corrections and comments
are welcome.

That's it.

Have fun.
