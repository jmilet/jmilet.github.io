---
layout: post
title:  "Mnesia Basics"
date:   2014-05-05 21:54:00
categories: erlang mnesia
---

I wanted to document this basic Mnesia module.

It's pretty simple:

* A simple record.
* The nodos() function returns a list of replica nodes.
* The install() function creates the schema in each node. It's
interesting to note that it starts and stops Mnesia at all nodes
via an rpc call and that "disc_copies" indicates that the "friend"
table will be stored in disk (it could be in memory for some nodes and
in disk for others).
* The start() function starts Mnesia up at all three nodes, also
waits upto 5 seconds for the tables to come up.
* The insert() function inserts N elements in the table.


{% highlight erlang %}
-module(mnesia_test).
-export([install/0, start/0, insert/1]).

-record(friends, {name, surname}).

nodos() ->
    ['a@192.168.1.36', 'b@192.168.1.36', 'c@192.168.1.102'].

install() ->
    Nodes = nodos(),
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(friends,
                        [{attributes, record_info(fields, friends)},
                         {disc_copies, Nodes}]),
    rpc:multicall(Nodes, application, stop, [mnesia]).

start() ->
    Nodes = nodos(),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:wait_for_tables([friends], 5000).

insert(0) ->
    ok;
insert(N) ->
    mnesia:dirty_write({friends, N, N}),
    timer:sleep(200),
    insert(N - 1).
{% endhighlight erlang %}

Let's set up our database cluster. We need to run all three nodes.
For example, for node 'a'.

{% highlight bash %}
$ erl -name a@192.168.1.36 -setcookie abc
{% endhighlight bash %}

Note the node name and the cookie. The cookie must match in all the nodes.

Once the three nodes are running let's create the schema and
start Mnesia at all of them.

{% highlight bash %}
$ erl -name a@192.168.1.36 -setcookie abc
Erlang R16B02 (erts-5.10.3) [source-b44b726] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V5.10.3  (abort with ^G)
(a@192.168.1.36)1> c(mnesia_test).
{ok,mnesia_test}
(a@192.168.1.36)2> mnesia_test:install().

=INFO REPORT==== 5-May-2014::22:27:25 ===
    application: mnesia
    exited: stopped
    type: temporary
{[ok,ok,ok],[]}
(a@192.168.1.36)3> mnesia_test:start().
ok
(a@192.168.1.36)4>
{% endhighlight bash %}

Now we have our distributed database up. Let's insert some data.

{% highlight bash %}
(a@192.168.1.36)4> spawn(fun() -> mnesia_test:insert(10) end).
<0.153.0>
(a@192.168.1.36)5>
{% endhighlight bash %}

We can go to node 'c' and check our new data.

{% highlight bash %}
(c@192.168.1.102)1> mnesia:dirty_all_keys(friends).
[1,10,2,3,8,4,5,7,6,9]
(c@192.168.1.102)2>
{% endhighlight bash %}

We can return to node 'a' again to insert a process PID in our
database.

{% highlight bash %}
(a@192.168.1.36)5> mnesia:dirty_write({friends, 2000, spawn(fun() -> receive A -> io:format("~p~n", [A]) end end)}).
ok
(a@192.168.1.36)6> mnesia:dirty_read({friends, 2000}).
[{friends,2000,<0.162.0>}]
(a@192.168.1.36)7>
{% endhighlight bash %}

Notice the difference if we read {friends, 2000} from node 'c'.

{% highlight bash %}
(c@192.168.1.102)2> mnesia:dirty_read({friends, 2000}).
[{friends,2000,<5981.162.0>}]
(c@192.168.1.102)3>
{% endhighlight bash %}

Do you see the difference with the PID's first number? Yes, it's the node,
the second one is the process.

Let's use it from node 'c'.

{% highlight bash %}
(c@192.168.1.102)3> [{_, _, Process}] = mnesia:dirty_read({friends, 2000}).
[{friends,2000,<5981.162.0>}]
(c@192.168.1.102)4> Process ! hola.
hola
(c@192.168.1.102)5>
{% endhighlight bash %}

Check the console in node 'a'. It works.

Let's clear the table.

{% highlight bash %}
(a@192.168.1.36)21> mnesia:clear_table(friends).
{atomic,ok}
(a@192.168.1.36)22>
{% endhighlight bash %}

Now, let's shut node 'c' down.

{% highlight bash %}
(a@192.168.1.36)22> mnesia:info().
---> Processes holding locks <---
---> Processes waiting for locks <---
---> Participant transactions <---
---> Coordinator transactions <---
---> Uncertain transactions <---
---> Active tables <---
friends        : with 0        records occupying 305      words of mem
schema         : with 2        records occupying 537      words of mem
===> System info in version "4.9", debug level = none <===
opt_disc. Directory "/Users/juan/Mnesia.a@192.168.1.36" is used.
use fallback at restart = false
running db nodes   = ['b@192.168.1.36','a@192.168.1.36']
stopped db nodes   = ['c@192.168.1.102']
master node tables = []
remote             = []
ram_copies         = []
disc_copies        = [friends,schema]
disc_only_copies   = []
[{'a@192.168.1.36',disc_copies},{'b@192.168.1.36',disc_copies}] = [schema,
                                                                   friends]
12 transactions committed, 0 aborted, 1 restarted, 101 logged to disc
0 held locks, 0 in queue; 0 local transactions, 0 remote
0 transactions waits for other nodes: []
ok
(a@192.168.1.36)23>
{% endhighlight bash %}

Note the stopped nodes.

Let's insert 20 rows from node 'a'.

{% highlight bash %}
(a@192.168.1.36)23> mnesia_test:insert(20).
ok
(a@192.168.1.36)24>
{% endhighlight bash %}

Let's start node 'c' up.

{% highlight bash %}
$ erl -name c@192.168.1.102 -setcookie abc
Erlang/OTP 17 [RELEASE CANDIDATE 2] [erts-6.0] [source] [async-threads:10] [kernel-poll:false]

Eshell V6.0  (abort with ^G)
(c@192.168.1.102)1> application:start(mnesia).
ok
(c@192.168.1.102)2> length(mnesia:dirty_all_keys(friends)).
20
(c@192.168.1.102)3>
{% endhighlight bash %}

The data is there.

Have fun.

