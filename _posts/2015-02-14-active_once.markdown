---
layout: post
title:  "gen_tcp and active_once"
date:   2015-02-15 12:00:00
categories: erlang
---

I wanted to understand and test the active\_once option used with the
gen\_tcp module. I started with a few lines of code and ended up with
three supervisors, one gen\_fsm and one gen\_server.

This is the documentation of gen\_tcp and active\_once's prove of
concept. As I always say these are just my learning notes and I can be
wrong. So check the OTP documentation and the fantastic books that can
be found on this topic.

# The goal

Something simple. Just to see active_once control flow in
action. Maybe this time the simple prove of concept rule of this blog
has been broken. The example is not as simple as it used to be, but I
think that at the end it's been worth it.

# Active and not active sockets

The gen_tcp module offers two ways to read sockets streams, active and
none active sockets. The latter are the classical way. Using the
recv/2 and recv/3 functions the process decides when to read from the
input stream. This way the process is in charge of controlling the
input data flow.

On the other hand, active sockets allow to receive the stream as
Erlang messages. The problem with this approach is that the other end
process can flood our process's queue as no control flow policy is
applied.

But there's a way to solve this problem. By setting the active\_once
option to true the process reads one time from the socket, receiving
the data as an Erlang message. Once the message is received the socket
returns to its non active form, stopping the writing process if the
buffer is full. The process can set the active\_once flag again to
receive another Erlang message from the socket to repeat the cycle.

# The controlling process

It's the process which receives the Erlang messages from the socket
stream when the socket is set to active or active\_once. By default
it's the process which accepts the socket.

As we'll see, the controlling process can be changed with the
controlling\_process/2 function.

# The structure of the prove of concept

Actually, it's quite simple.

```
                 main_sup
				    |
	    +-----------------------+
        |                       |
		|                       |
   listener_sup      +---->  worker_sup    (*) The accept process spawns a worker
        |          /            |              on the accepted socket
		|        /              |
 (n) accept_fsm          (m) worker_gen

```

**main_sup**: The main supervisor.

**listener_sup**: The accepting connection process's supervisor.

**worker_sup**: The reader worker's supervisor.

**accept_fsm**: The accepting connections process.

**worker_gen**: The reader worker.

<br>
# main_sup - The main supervisor

It uses permanent one\_on\_one restart strategy. It has two children
which also are supervisors.

```Erlang
-module(main_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:format("main_sup:init...~n"),

    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = supervisor,

    ListenerSup = {'listener_sup',
		   {'listener_sup', start_link, []},
		   Restart, Shutdown, Type, ['listener_sup']},

    WorkerSup = {'worker_sup',
		   {'worker_sup', start_link, []},
		   Restart, Shutdown, Type, ['worker_sup']},

    ChildList = [ListenerSup, WorkerSup],

    {ok, {SupFlags, ChildList}}.
```
<br>
# listener_sup - The accepting connection process's supervisor

It uses permanent one\_on\_one restart strategy. Its children are
accept_fsm processes. Three of them are started in order to be ready
for a big connection boost.

The listen socket is created in the init function with its active
attribute set to false, we want control flow by default.

It's worth to mention that the child list is built with a list
comprehension where each id is created with the build\_label function
resulting in an id of the form accept\_fsm\_n where n is the process's
order.

```Erlang
-module(listener_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:format("listener_sup:init...~n"),

    {ok, LSocket} = gen_tcp:listen(2000, [{active, false}]),

    NumberOfWorkers = 3,
    RestartStrategy = one_for_one,
    MaxRestarts = 100,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    ChildList = [{build_label("accept_fsm_", C),
		  {'accept_fsm', start_link, [LSocket]},
		  Restart, Shutdown, Type, ['accept_fsm']}
		 || C <- lists:seq(1, NumberOfWorkers)],

    {ok, {SupFlags, ChildList}}.

build_label(Name, C) ->
    io_lib:format("~s~w", [Name, C]).
```
<br>
# worker_sup - The reader worker's supervisor

It uses temporary simple\_on\_on\_one restart strategy to start
processes on demand.  Its children are worker\_gen processes. It
exports the function start\_child/1 to start a worker\_gen child where
the new accepted socket is passed in. Also it sets the new children
process as the controlling process of the socket so it can receive the
Erlang messages stream.

```Erlang
-module(worker_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1]).
-export([init/1]).


start_link() ->
    io:format("listener_sup:start_link...~n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(ASocket) ->
    {ok, Pid} = supervisor:start_child(?MODULE, [ASocket]),
    gen_tcp:controlling_process(ASocket, Pid),
    {ok, Pid}.

init([]) ->
    io:format("listener_sup:init...~n"),

    RestartStrategy = simple_one_for_one,
    MaxRestarts = 100,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = temporary,
    Shutdown = 2000,
    Type = worker,

    AChild = {'worker_gen', {'worker_gen', start_link, []},
	      Restart, Shutdown, Type, ['worker_gen']},

    {ok, {SupFlags, [AChild]}}.
```
<br>
# accept_fsm - The accepting connection process

This process is implemented as a FSM of just one state, the accept
state. It receives a listening socket and accepts connections on
it. When a new connection arrives, the process sets the socket as
active\_once and asks the worker_sup for a new process passing it the
new accepted socket. Then it returns to the accept state again.

```Erlang
-module(accept_fsm).

-behaviour(gen_fsm).

-export([start_link/1]).
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([accept/2]).

start_link(LSocket) ->
    io:format("accept_fsm:start_link...~n"),
    gen_fsm:start_link(?MODULE, [LSocket], []).

init([LSocket]) ->
    io:format("accept_fsm:init...~n"),
    gen_fsm:send_event(self(), accept),
    {ok, accept, LSocket}.

% Events.
accept(accept, LSocket) ->
    io:format("accept_fsm:accept...~n"),
    {ok, ASocket} = gen_tcp:accept(LSocket),
    inet:setopts(ASocket, [{active, once}, {packet, line}]),
    worker_sup:start_child(ASocket),
    gen_fsm:send_event(self(), accept),
    {next_state, accept, LSocket}.

% All events.
handle_event(accept, _StateName, State) ->
    {next_state, accept, State}.

handle_sync_event(_Any, _From, _StateName, State) ->
    {reply, ok, accept, State}.

% OTP messeges.
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
```
<br>
# worker_gen - The socket reader worker

It's where the prove of concept sits. It just prints the received data
to the standard output.

In order to play with the control flow a delay is introduced between
each read. This way the reading speed will be much lower than the
writing one, forcing the writer process to stop as the writing buffer
gets full.

Note how the active_once option is set after each read.

```Erlang
-module(worker_gen).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

start_link(ASocket) ->
    io:format("worker_gen:start_link...~n"),
    gen_server:start_link(?MODULE, [ASocket], []).

init([ASocket]) ->
    io:format("worker_gen:init...~n"),
    {ok, ASocket}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({tcp, _S, Data}, ASocket) ->
    io:format("~p~n", [Data]),
    inet:setopts(ASocket, [{active, once}, {packet, line}]),
    timer:sleep(100),
    {noreply, ASocket};

handle_info({tcp_closed, _S}, ASocket) ->
    io:format("Closed...~n"),
    {stop, normal, ASocket};

handle_info(timeout, ASocket) ->
    io:format("Closed...~n"),
    {stop, normal, ASocket}.

terminate(_Reason, ASocket) ->
    io:format("Terminated...~n"),
    gen_tcp:close(ASocket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```
<br>
# The code working

Let's see how the code works.

```Erlang
$ erl
Erlang/OTP 17 [erts-6.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe]
[kernel-poll:false]

Eshell V6.1  (abort with ^G)
1> l(main_sup).
{module,main_sup}
2> main_sup:start_link().
main_sup:init...
listener_sup:init...
accept_fsm:start_link...
accept_fsm:init...
accept_fsm:accept...
accept_fsm:start_link...
accept_fsm:init...
accept_fsm:accept...
accept_fsm:start_link...
accept_fsm:init...
accept_fsm:accept...
listener_sup:start_link...
listener_sup:init...
{ok,<0.35.0>}
3>
```

Let's connect a client.

```Erlang
...
accept_fsm:accept...
listener_sup:start_link...
listener_sup:init...
{ok,<0.35.0>}
worker_gen:start_link...
worker_gen:init...
accept_fsm:accept...
3>
```

And write some text from the client.

```Erlang
accept_fsm:accept...
listener_sup:start_link...
listener_sup:init...
{ok,<0.35.0>}
worker_gen:start_link...
worker_gen:init...
accept_fsm:accept...
"Hello World!!!\r\n"
3>
```

Let's write a big amount of data and time it.

```
$ time man ls|nc -c localhost 2000

real	0m39.517s
user	0m0.083s
sys	0m0.019s
$
```

Let's do the same, but now removing the lag.

```
$ time man ls|nc -c localhost 2000

real	0m0.100s
user	0m0.085s
sys	0m0.020s
$
```

This let us see how the writing process stops waiting for free space
in the buffer.

After stressing the server a little bit we check that there is no
process leak.

```Erlang
3> supervisor:count_children(main_sup).
[{specs,2},{active,2},{supervisors,2},{workers,0}]
4> supervisor:count_children(listener_sup).
[{specs,3},{active,3},{supervisors,0},{workers,3}]
5> supervisor:count_children(worker_sup).
[{specs,1},{active,0},{supervisors,0},{workers,0}]
6>
```

I'm not quite sure whether this solution has any problem I'm not aware
of.  I've tried to follow the patters I've learnt reading the Erlang
and Elixir books that cover the topic. I could have got something
wrong so, please, read those books and get accuarate explanations
about how these things actually work.

Please, contact me for any corrections, comments or suggestions. I'd
love to hear from you.

That's it.

Have fun.

