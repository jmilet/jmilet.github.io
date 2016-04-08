---
layout: post
title:  "Release with iex"
date:   2016-04-08 00:00:00
categories: release elixir erlang
---

In this post I want to document how to make an Erlang release. Actually,
it's nothing new and it's perfectly explained [here](http://erlang.org/doc/design_principles/release_structure.html),
but I wanted to do it myself. This is the result.

Let's begin with three toy modules: an application, a supervisor and a gen_server.

Our app is :testrel in version 0.0.1, our main module Testrel.

```elixir
defmodule Testrel.Mixfile do
  use Mix.Project

  def project do
    [app: :testrel,
     version: "0.0.1",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [mod: {Testrel, []},
    applications: [:logger]]
  end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options
  defp deps do
    []
  end
end
```

The application module.

```elixir
defmodule Testrel do
  use Application

  def start(_type, _args) do
    TestrelSup.start_link()
  end
end
```

The supervisor module.

```elixir
defmodule TestrelSup do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    children = [
      worker(TestrelGen, [[:hello]])
    ]

    supervise(children, strategy: :one_for_one)
  end
end
```

The gen_server module.

```elixir
defmodule TestrelGen do
  def start_link(state, opts \\ []) do
    GenServer.start_link(__MODULE__, state, opts)
  end

  def init(_) do
    IO.puts "TestrelGen.init starting..."
    {:ok, :nostate}
  end
end
```

Let's test that it works. It does, see the shiny message.

```elixir
$ iex -S mix
Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe]
[kernel-poll:false] [dtrace]

TestrelGen.init starting...
Interactive Elixir (1.2.3) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)>
```

Let's compile for production.

```elixir
$ ENV_MIX=prod mix compile
Compiled lib/testrel_gen.ex
Compiled lib/testrel_sup.ex
Compiled lib/testrel.ex
Generated testrel app
Consolidated List.Chars
Consolidated String.Chars
Consolidated Collectable
Consolidated Enumerable
Consolidated IEx.Info
Consolidated Inspect
$
```

Let's move to the directory where the .beam files are created. At this point I need some
help. Is this directory the right place to build the release? **Please, contact me if you know the answer.**

```
$ cd ~/release/testrel/_build/prod/lib/testrel/ebin
```

The next step is to create a .rel file. This file defines our application dependencies.
Note that it's actual Erlang code.

```erlang
{release,
 {"testrel", "0.0.1"},
 {erts, "5.3"},
 [{kernel, "4.2"},
  {stdlib, "2.8"},
  {sasl, "2.7"},
  {elixir, "1.2.3"},
  {logger, "1.2.3"},
  {compiler, "6.0.3"},
  {testrel, "0.0.1"}]
}.
```

Now we need to create the script that will be used by the Erlang start system. It comes
both in binary and source format.

```elixir
iex(1)> :systools.make_script('testrel', [:local])
:ok
iex(2)>
```

This files are.

```elixir
$ ls testrel.boot testrel.script
testrel.boot   testrel.script
$
```

Now we can start our system from the boot script.

```
$ erl -boot testrel
Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe]
[kernel-poll:false] [dtrace]


=PROGRESS REPORT==== 8-Apr-2016::22:12:05 ===
          supervisor: {local,sasl_safe_sup}
             started: [{pid,<0.36.0>},
                       {id,alarm_handler},
                       {mfargs,{alarm_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 8-Apr-2016::22:12:05 ===
          supervisor: {local,sasl_safe_sup}
             started: [{pid,<0.37.0>},
                       {id,overload},
                       {mfargs,{overload,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 8-Apr-2016::22:12:05 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.35.0>},
                       {id,sasl_safe_sup},
                       {mfargs,
                           {supervisor,start_link,
                               [{local,sasl_safe_sup},sasl,safe]}},
                       {restart_type,permanent},
                       {shutdown,infinity},
                       {child_type,supervisor}]

=PROGRESS REPORT==== 8-Apr-2016::22:12:05 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.38.0>},
                       {id,release_handler},
                       {mfargs,{release_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 8-Apr-2016::22:12:05 ===
         application: sasl
          started_at: nonode@nohost

=PROGRESS REPORT==== 8-Apr-2016::22:12:05 ===
         application: compiler
          started_at: nonode@nohost

=PROGRESS REPORT==== 8-Apr-2016::22:12:05 ===
          supervisor: {local,elixir_sup}
             started: [{pid,<0.45.0>},
                       {id,elixir_config},
                       {mfargs,{elixir_config,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 8-Apr-2016::22:12:05 ===
          supervisor: {local,elixir_sup}
             started: [{pid,<0.46.0>},
                       {id,elixir_code_server},
                       {mfargs,{elixir_code_server,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 8-Apr-2016::22:12:05 ===
          supervisor: {local,elixir_sup}
             started: [{pid,<0.47.0>},
                       {id,elixir_counter},
                       {mfargs,{elixir_counter,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 8-Apr-2016::22:12:05 ===
         application: elixir
          started_at: nonode@nohost

=PROGRESS REPORT==== 8-Apr-2016::22:12:05 ===
          supervisor: {local,'Elixir.Logger.Supervisor'}
             started: [{pid,<0.52.0>},
                       {id,'Elixir.GenEvent'},
                       {mfargs,
                           {'Elixir.GenEvent',start_link,
                               [[{name,'Elixir.Logger'}]]}},
                       {restart_type,permanent},
                       {shutdown,5000},
                       {child_type,worker}]

=PROGRESS REPORT==== 8-Apr-2016::22:12:05 ===
          supervisor: {local,'Elixir.Logger.Supervisor'}
             started: [{pid,<0.53.0>},
                       {id,'Elixir.Logger.Config'},
                       {mfargs,
                           {'Elixir.Logger.Watcher',watcher,
                               ['Elixir.Logger','Elixir.Logger.Config',[]]}},
                       {restart_type,permanent},
                       {shutdown,5000},
                       {child_type,worker}]

=PROGRESS REPORT==== 8-Apr-2016::22:12:05 ===
          supervisor: {local,'Elixir.Logger.Supervisor'}
             started: [{pid,<0.54.0>},
                       {id,'Elixir.Logger.Watcher'},
                       {mfargs,
                           {'Elixir.Logger.Watcher',start_link,
                               ['Elixir.Logger.Config',handlers,[]]}},
                       {restart_type,permanent},
                       {shutdown,infinity},
                       {child_type,supervisor}]

=PROGRESS REPORT==== 8-Apr-2016::22:12:05 ===
          supervisor: {local,'Elixir.Logger.Supervisor'}
             started: [{pid,<0.56.0>},
                       {id,'Elixir.Logger.ErrorHandler'},
                       {mfargs,
                           {'Elixir.Logger.Watcher',watcher,
                               [error_logger,'Elixir.Logger.ErrorHandler',
                                {true,false,500},
                                link]}},
                       {restart_type,permanent},
                       {shutdown,5000},
                       {child_type,worker}]

=PROGRESS REPORT==== 8-Apr-2016::22:12:05 ===
         application: logger
          started_at: nonode@nohost
TestrelGen.init starting...

=PROGRESS REPORT==== 8-Apr-2016::22:12:05 ===
          supervisor: {<0.60.0>,'Elixir.TestrelSup'}
             started: [{pid,<0.61.0>},
                       {id,'Elixir.TestrelGen'},
                       {mfargs,{'Elixir.TestrelGen',start_link,[[hello]]}},
                       {restart_type,permanent},
                       {shutdown,5000},
                       {child_type,worker}]

=PROGRESS REPORT==== 8-Apr-2016::22:12:05 ===
         application: testrel
          started_at: nonode@nohost
Eshell V7.3  (abort with ^G)
1>
```

Note our shiny message near the end of the trace.

Let's now build the final tar file that can be distributed.

```
$ iex
Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe]
[kernel-poll:false] [dtrace]

Interactive Elixir (1.2.3) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> :systools.make_tar('testrel')
:ok
iex(2)>
```

Our file is.

```
$ ls testrel.tar.gz
testrel.tar.gz
$
```

Let's copy the tar file into another directory, untar it, and start the application up.

```
~/t $ cp ~/release/testrel/_build/prod/lib/testrel/ebin/testrel.tar.gz .
~/t $ tar xvfz testrel.tar.gz > /dev/null 2>&1
~/t $ ls
lib            releases       testrel.tar.gz
~/t $
```

And run the app again.

```
~/t $ erl -boot releases/0.0.1/start -env ERL_LIBS ./lib
Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe]
[kernel-poll:false] [dtrace]


=PROGRESS REPORT==== 8-Apr-2016::22:19:20 ===
          supervisor: {local,sasl_safe_sup}
             started: [{pid,<0.36.0>},
                       {id,alarm_handler},
                       {mfargs,{alarm_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 8-Apr-2016::22:19:20 ===
          supervisor: {local,sasl_safe_sup}
             started: [{pid,<0.37.0>},
                       {id,overload},
                       {mfargs,{overload,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 8-Apr-2016::22:19:20 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.35.0>},
                       {id,sasl_safe_sup},
                       {mfargs,
                           {supervisor,start_link,
                               [{local,sasl_safe_sup},sasl,safe]}},
                       {restart_type,permanent},
                       {shutdown,infinity},
                       {child_type,supervisor}]

=PROGRESS REPORT==== 8-Apr-2016::22:19:20 ===
          supervisor: {local,sasl_sup}
             started: [{pid,<0.38.0>},
                       {id,release_handler},
                       {mfargs,{release_handler,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 8-Apr-2016::22:19:20 ===
         application: sasl
          started_at: nonode@nohost

=PROGRESS REPORT==== 8-Apr-2016::22:19:20 ===
         application: compiler
          started_at: nonode@nohost

=PROGRESS REPORT==== 8-Apr-2016::22:19:20 ===
          supervisor: {local,elixir_sup}
             started: [{pid,<0.45.0>},
                       {id,elixir_config},
                       {mfargs,{elixir_config,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 8-Apr-2016::22:19:20 ===
          supervisor: {local,elixir_sup}
             started: [{pid,<0.46.0>},
                       {id,elixir_code_server},
                       {mfargs,{elixir_code_server,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 8-Apr-2016::22:19:20 ===
          supervisor: {local,elixir_sup}
             started: [{pid,<0.47.0>},
                       {id,elixir_counter},
                       {mfargs,{elixir_counter,start_link,[]}},
                       {restart_type,permanent},
                       {shutdown,2000},
                       {child_type,worker}]

=PROGRESS REPORT==== 8-Apr-2016::22:19:20 ===
         application: elixir
          started_at: nonode@nohost

=PROGRESS REPORT==== 8-Apr-2016::22:19:20 ===
          supervisor: {local,'Elixir.Logger.Supervisor'}
             started: [{pid,<0.52.0>},
                       {id,'Elixir.GenEvent'},
                       {mfargs,
                           {'Elixir.GenEvent',start_link,
                               [[{name,'Elixir.Logger'}]]}},
                       {restart_type,permanent},
                       {shutdown,5000},
                       {child_type,worker}]

=PROGRESS REPORT==== 8-Apr-2016::22:19:20 ===
          supervisor: {local,'Elixir.Logger.Supervisor'}
             started: [{pid,<0.53.0>},
                       {id,'Elixir.Logger.Config'},
                       {mfargs,
                           {'Elixir.Logger.Watcher',watcher,
                               ['Elixir.Logger','Elixir.Logger.Config',[]]}},
                       {restart_type,permanent},
                       {shutdown,5000},
                       {child_type,worker}]

=PROGRESS REPORT==== 8-Apr-2016::22:19:20 ===
          supervisor: {local,'Elixir.Logger.Supervisor'}
             started: [{pid,<0.54.0>},
                       {id,'Elixir.Logger.Watcher'},
                       {mfargs,
                           {'Elixir.Logger.Watcher',start_link,
                               ['Elixir.Logger.Config',handlers,[]]}},
                       {restart_type,permanent},
                       {shutdown,infinity},
                       {child_type,supervisor}]

=PROGRESS REPORT==== 8-Apr-2016::22:19:20 ===
          supervisor: {local,'Elixir.Logger.Supervisor'}
             started: [{pid,<0.56.0>},
                       {id,'Elixir.Logger.ErrorHandler'},
                       {mfargs,
                           {'Elixir.Logger.Watcher',watcher,
                               [error_logger,'Elixir.Logger.ErrorHandler',
                                {true,false,500},
                                link]}},
                       {restart_type,permanent},
                       {shutdown,5000},
                       {child_type,worker}]

=PROGRESS REPORT==== 8-Apr-2016::22:19:20 ===
         application: logger
          started_at: nonode@nohost
TestrelGen.init starting...

=PROGRESS REPORT==== 8-Apr-2016::22:19:20 ===
          supervisor: {<0.60.0>,'Elixir.TestrelSup'}
             started: [{pid,<0.61.0>},
                       {id,'Elixir.TestrelGen'},
                       {mfargs,{'Elixir.TestrelGen',start_link,[[hello]]}},
                       {restart_type,permanent},
                       {shutdown,5000},
                       {child_type,worker}]

=PROGRESS REPORT==== 8-Apr-2016::22:19:20 ===
         application: testrel
          started_at: nonode@nohost
Eshell V7.3  (abort with ^G)
1>
```

Note again our shiny message.

Just one last thing. What's in our directory?

```elixir
~/t $ find . -type d
.
./lib
./lib/compiler-6.0.3
./lib/compiler-6.0.3/ebin
./lib/elixir-1.2.3
./lib/elixir-1.2.3/ebin
./lib/kernel-4.2
./lib/kernel-4.2/ebin
./lib/logger-1.2.3
./lib/logger-1.2.3/ebin
./lib/sasl-2.7
./lib/sasl-2.7/ebin
./lib/stdlib-2.8
./lib/stdlib-2.8/ebin
./lib/testrel-0.0.1
./lib/testrel-0.0.1/ebin
./releases
./releases/0.0.1
~ t $
```

That's it.

Corrections and improvements are welcome.

Have fun.
