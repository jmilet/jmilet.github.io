---
layout: post
title:  "Dialyzer Basics"
date:   2016-03-13 00:00:00
categories: Dialyzer elixir erlang
---

These are my notes about Dialyzer meant to help remember myself how
it works. You can find a complete definition and instructions here
[http://erlang.org/doc/man/dialyzer.html](http://erlang.org/doc/man/dialyzer.html).

As usual we'll use a very simple example to build our explanation on.

# Types

Erlang is a strongly dynamic typed language so type checks are done at runtime.
Even though its dynamism has several advantages it's convenient to be able to
check types at compile time. This is where Dialyzer comes in. This tool
allows us to analyze our code in order to detect type mismatches and other
kind of problems before we run our programs.

Let's write a toy function that receives an *user_id* and a *thing_id* and return
them as a tuple where the order of the elements is important. Both values are
integers so it's easy to mistakenly cross them and pass an *user_id* where
a *thing_id* is expected and the other way around.

This is the example.

```erlang
-module(types1).
-export([foo/0]).

tuplefy(ThingId, UserId) ->
  {ThingId, UserId}.

foo() ->
  tuplefy(1, 2).
```
Looking at the execution we find that 1 and 2 are our id's but it's not easy
to distinguish each kind of id.

```erlang
$ erl
Erlang/OTP 18 [erts-7.1] [source] [64-bit] [smp:4:4] [async-threads:10]
[hipe] [kernel-poll:false]

Eshell V7.1  (abort with ^G)
1> c(types1).
{ok,types1}
2> types1:foo().
{1,2}
```

# Tag the types

We can improve things a little bit by tagging our parameters.

{% raw %}
```erlang
-module(types2).
-export([foo/0]).

tuplefy({thing_id, ThingId}, {user_id, UserId}) ->
  {{thing_id, ThingId}, {user_id, UserId}}.

foo() ->
  tuplefy({thing_id, 1}, {user_id, 2}).
```
{% endraw %}

It works and we get some clue about the type of parameters we are dealing with.

{% raw %}
```erlang
$ erl
Erlang/OTP 18 [erts-7.1] [source] [64-bit] [smp:4:4] [async-threads:10]
[hipe] [kernel-poll:false]

Eshell V7.1  (abort with ^G)
1> c(types2).
{ok,types2}
2> types2:foo().
{{thing_id,1},{user_id,2}}
3>
```
{% endraw %}

This allows us to detect type mismatches at runtime. For example, this is
the result of crossing both parameters in foo().

{% raw %}
```erlang
-module(types3).
-export([foo/0]).

tuplefy({thing_id, ThingId}, {user_id, UserId}) ->
  {{thing_id, ThingId}, {user_id, UserId}}.

foo() ->
  tuplefy({user_id, 2}, {thing_id, 1}). %% <<<<<< Cross parameters
```
{% endraw %}

The runtime error.

{% raw %}
```erlang
$ erl
Erlang/OTP 18 [erts-7.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe]
[kernel-poll:false]

Eshell V7.1  (abort with ^G)
1> c(types3).
{ok,types3}
2> types3:foo().
** exception error: no function clause matching types3:tuplefy({user_id,2},
  {thing_id,1}) (types3.erl, line 4)
3>
```
{% endraw %}

But we want the catch the error way before execution. Let's begin with
Dialyzer.

# Building the PLT

Before our final examples we'll prepare our Erlang installation to work with
Dialyzer. In order to work, Dialyzer needs to build a type's repository which is
created at $HOME/.dialyzer_plt. This process takes a while, be patient.

```
$ dialyzer --build_plt --apps erts kernel stdlib
  Compiling some key modules to native code... done in 0m0.32s
  Creating PLT /Users/jmimora/.dialyzer_plt ...
Unknown functions:
  compile:file/2
  compile:forms/2
  compile:noenv_forms/2
  compile:output_generated/1
  crypto:block_decrypt/4
  crypto:start/0
Unknown types:
  compile:option/0
 done in 0m57.99s
done (passed successfully)
```

# Type and function definition

So now we'll define types for our two ids and specs for our two functions.

```erlang
-module(types4).
-export([foo/0]).

-type thing_id()::{thing_id, integer()}.
-type user_id()::{user_id, integer()}.

-spec tuplefy(ThingId::thing_id(), UserId::user_id()) -> {thing_id(), user_id()}.
-spec foo() -> {thing_id(), user_id()}.

tuplefy(ThingId, UserId) ->
  {ThingId, UserId}.

foo() ->
  tuplefy({thing_id, 1}, {user_id, 2}).
```

We find nothing new at runtime.

{% raw %}
```erlang
$ erl
Erlang/OTP 18 [erts-7.1] [source] [64-bit] [smp:4:4] [async-threads:10]
[hipe] [kernel-poll:false]

Eshell V7.1  (abort with ^G)
1> c(types4).
{ok,types4}
2> types4:foo().
{{thing_id,1},{user_id,2}}
```
{% endraw %}

But now, unlike we did before, we are going to cross the returned values, not
the calling parameters.

```erlang
-module(types5).
-export([foo/0]).

-type thing_id()::{thing_id, integer()}.
-type user_id()::{user_id, integer()}.

-spec tuplefy(ThingId::thing_id(), UserId::user_id()) -> {thing_id(), user_id()}.
-spec foo() -> {thing_id(), user_id()}.

tuplefy(ThingId, UserId) ->
  {UserId, ThingId}.   %% <<<<<<< Cross parameters.

foo() ->
  tuplefy({thing_id, 1}, {user_id, 2}).
```

Obviously, we get the values in the opposite order.

{% raw %}
```erlang
$ erl
Erlang/OTP 18 [erts-7.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe]
[kernel-poll:false]

Eshell V7.1  (abort with ^G)
1> c(types5).
{ok,types5}
2> types5:foo().
{{user_id,2},{thing_id,1}}
```
{% endraw %}

But, Dialyzer can realize that there exists a type inconsistence between
the actual return and the function specification before the program is run.

{% raw %}
```
$ dialyzer types5.erl
  Checking whether the PLT /Users/jmimora/.dialyzer_plt is up-to-date... yes
  Proceeding with analysis...
types5.erl:7: The contract types5:tuplefy(ThingId::thing_id(),UserId::user_id())
-> {thing_id(),user_id()} cannot be right because the inferred return for
tuplefy({'thing_id', 1},{'user_id',2}) on line 14 is {{'user_id',2},{'thing_id',1}}
types5.erl:13: Function foo/0 has no local return
 done in 0m0.69s
done (warnings were emitted)
```
{% endraw %}

Compare it with a clean analysis.

```
$ dialyzer types4.erl
  Checking whether the PLT /Users/jmimora/.dialyzer_plt is up-to-date... yes
  Proceeding with analysis... done in 0m0.69s
done (passed successfully)
```

Just one final thing. Let's remove the foo() call on types5.erl and name the
new file types7.erl. We'll keep the code commented to see the differences.

```erlang
-module(types7).
% -export([foo/0]).

-type thing_id()::{thing_id, integer()}.
-type user_id()::{user_id, integer()}.

-spec tuplefy(ThingId::thing_id(), UserId::user_id()) -> {thing_id(), user_id()}.
% -spec foo() -> {thing_id(), user_id()}.

tuplefy(ThingId, UserId) ->
  {UserId, ThingId}.   %% <<<<<<< Cross parameters.

% foo() ->
%   tuplefy({thing_id, 1}, {user_id, 2}).
```

Let's run dialyzer again.

```
$ dialyzer types7.erl
  Checking whether the PLT /Users/jmimora/.dialyzer_plt is up-to-date... yes
  Proceeding with analysis...
types7.erl:10: Function tuplefy/2 will never be called
 done in 0m0.69s
done (warnings were emitted)
```

Dialyzer warns us about the never called function, but it can't say anything
about the types, because *ThindId* and *UserId* actually are just names that
will be bound to some data, but a this point, Dialyzer doesn't know what its types
could be. It's the data flow what Dialyzer checks so it needs a data entry point
to analyze the involved types.

That's it.

Corrections and improvements are welcome.

Have fun.
