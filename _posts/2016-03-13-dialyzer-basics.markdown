---
layout: post
title:  "Dialyzer Basics"
date:   2016-03-13 00:00:00
categories: Dialyzer elixir erlang
---

These are my notes about Dialyzer mean to help me to remember how
it works. You can find a complete definition and instructions here
[http://erlang.org/doc/man/dialyzer.html](http://erlang.org/doc/man/dialyzer.html).

As usual we'll use a very simple example to build our explanation on.

# Types

Erlang is a strongly dynamic typed language so type checks are done at runtime.
Even though this dynamism has several advantages it's convenient to be able to
check types at compile time. This is where Dialyzer comes in. This tool
allows us to analyze our code in order to detect type mismatches and other
kind of problems.

Let's write a toy function that receives an *user_id* and a *thing_id* and return
them as a tuple where the order of the elements is important. Both values are
integers so it's easy to mistakenly cross them and pass an *user_id* where
a *thing_id* is expected and the other way around.

This is the example.

```Erlang
-module(types1).
-export([foo/0]).

tuplefy(ThingId, UserId) ->
  {ThingId, UserId}.

foo() ->
  tuplefy(1, 2).
```
Looking at the execution we find that 1 and 2 are our id's but it's not easy
to know which one is the *user_id* and which one is the *thing_id*.

```Erlang
$ erl
Erlang/OTP 18 [erts-7.1] [source] [64-bit] [smp:4:4] [async-threads:10]
[hipe] [kernel-poll:false]

Eshell V7.1  (abort with ^G)
1> c(types1).
{ok,types1}
2> types1:foo().
{1,2}
```

<space>
# Tag the types

We can improve things a little bit by tagging our parameters.

```Erlang
-module(types2).
-export([foo/0]).

tuplefy({thing_id, ThingId}, {user_id, UserId}) ->
  {ThingId, UserId}.

foo() ->
  tuplefy({thing_id, 1}, {user_id, 2}).
```
It works and we get some clue about the type of parameters we are passing in.

```Erlang
$ erl
Erlang/OTP 18 [erts-7.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe]
[kernel-poll:false]

Eshell V7.1  (abort with ^G)
1> c(types2).
{ok,types2}
2> types2:foo().
{1,2}
```

This allows to detect type mismatches at runtime. For example, this is
the result of crossing both parameters in foo().

```Erlang
-module(types3).
-export([foo/0]).

tuplefy({thing_id, ThingId}, {user_id, UserId}) ->
  {ThingId, UserId}.

foo() ->
  tuplefy({user_id, 2}, {thing_id, 1}).
```

The runtime error.

```Erlang
$ erl
Erlang/OTP 18 [erts-7.1] [source] [64-bit] [smp:4:4] [async-threads:10]
[hipe] [kernel-poll:false]

Eshell V7.1  (abort with ^G)
1> c(types3).
{ok,types3}
2> types3:foo().
** exception error: no function clause matching
                    types3:tuplefy({user_id,2},{thing_id,1}) (types3.erl, line 4)
```

But we want the catch the error way before execution. Let's begin with
Dialyzer.

# Building the PLT

Before our final example we'll prepare our Erlang installation to work with
Dialyzer. In order to work, it needs to build a type's repository which is
located at $HOME/.dialyzer_plt. This process takes a while, be patient.

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
<space>
# Type and function definition

So now we'll define types for our two ids and specs for our two functions. Note that
we have changed the return type of the tuplefy() function from integer to
*thind_id()* and *user_id()* respectively, in order to have a better example
to get a type mismatch.

```Erlang
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
```Erlang
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

But, what would happen if we cross the return parameters in tuplefy as we
previously did?

```Erlang
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
```Erlang
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

But, Dialyzer can help us to catch the type mismatch earlier.

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

Dialyzer has detected the type inconsistence between the function spec and
its actual return value. Compare it with a clean analysis.

```
$ dialyzer types4.erl
  Checking whether the PLT /Users/jmimora/.dialyzer_plt is up-to-date... yes
  Proceeding with analysis... done in 0m0.69s
done (passed successfully)
```

Just one final thing. Let's remove the foo() call to types5.erl and name it
types7.erl. We'll keep the code commented to see the differences.

```Erlang
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

Let's run dialyzer.

```
$ dialyzer types7.erl
  Checking whether the PLT /Users/jmimora/.dialyzer_plt is up-to-date... yes
  Proceeding with analysis...
types7.erl:10: Function tuplefy/2 will never be called
 done in 0m0.69s
done (warnings were emitted)
```

Dialyzer warns us about the never called function, but it can't say anything
about the types, because actually ThindId and UserId are just names bound
to some data and have no type information at this point. It's the data
flow what dialyzer checks.

That's it. Corrections and improvements are welcome.

Have fun.
