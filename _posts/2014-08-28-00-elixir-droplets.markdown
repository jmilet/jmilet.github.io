---
layout: post
title:  "Elixir Droplets"
date:   2014-08-28 20:19:00
categories: erlang elixir
---

The last 15 days I've been reading
[Elixir in Action](http://www.manning.com/juric/). This book is
amazing. I hightly recommend it. The book briefly explores Elixir as a
language with some Erlang pics. But where it really excels is in its
OTP's treatment and the patterns for building concurrents
systems. It's not a beginers book, you need some Erlang and FP
experience to take full adavantatge of the book.

Some time ago I tried to learn Elixir, but the additional syntax you
have to learn compared to Erlang seemed a heavy burden to me and it
always made me give up. But this book has changed my mind. Actually,
the syntax is very simimilar to Python's or Ruby's and the little things
that are different seem very coherent. In fact, Elixir brings the
power of Erlang with a fairly familiar syntax.

As self documentation and hoping it could help someone else here are
my notes. Take into account that they are writen from an Erlang and
Elixir beginer's perspective. The following is some syntactical stuff
I have found interesting.

# Keyword Lists

Maybe one of the best readibilty improvements over Erlang.

In Erlang.

```elixir
7> proplists:get_value(dos, [{uno, 1}, {dos, 2}]).
2
8>
```

In Elixir.

```elixir
iex(6)> prop = [{:uno, 1}, {:dos, 2}]
[uno: 1, dos: 2]
iex(7)> prop[:dos]
2
iex(8)>
```

But also can be writen more simply.

```elixir
iex(8)> prop = [uno: 1, dos: 2]
[uno: 1, dos: 2]
iex(9)> prop[:dos]
2
iex(10)>
```

But where it really shines is when used as a named parameter list
function call where we can omit the square brackets.

```elixir
iex(10)> defmodule Test do
...(10)> def foo(a, b), do: {a, b}
...(10)> end
{:module, Test,
 <<70, 79, 82, 49, 0, 0, 4, 128, 66, 69, 65, 77, 69, 120, 68, 99, 0,
 0, 0, 123, 131, 104, 2, 100, 0, 14, 101, 108, 105,120, 105, 114, 95,
 100, 111, 99, 115, 95, 118, 49, 108, 0, 0, 0, 2, 104, 2, ...>>,
 {:foo, 2}}
iex(11)> Test.foo 100, hola: 'que tal', bien: 'gracias'
{100, [hola: 'que tal', bien: 'gracias']}
iex(12)>
```

The magic thing is that **def foo(a, b), do: {a, b}** is using this
very same concept to receive the **do:** keyword as other "macros"
that define the language, for example **alias**, do. Love it.

# Structs

```elixir
iex(1)> defmodule Test do
...(1)> defstruct name: 'juan', surname: 'garcia'
...(1)> end
{:module, Test,
 <<70, 79, 82, 49, 0, 0, 4, 248, 66, 69, 65, 77, 69, 120, 68, 99, 0,
 0, 0, 99, 131, 104, 2, 100, 0, 14, 101, 108, 105, 120, 105, 114, 95,
 100, 111, 99, 115, 95, 118, 49, 108, 0, 0, 0, 2, 104, 2, ...>>,
[name: 'juan', surname: 'garcia']}
iex(2)> %Test{}
%Test{name: 'juan', surname: 'garcia'}
iex(3)> user = %Test{}
%Test{name: 'juan', surname: 'garcia'}
iex(4)> %Test{user|surname: 'perez'}
%Test{name: 'juan', surname: 'perez'}
iex(5)> user.name
'juan'
iex(6)>
```

# Maps

```elixir
iex(8)> d = %{:a => 1, :b => 2}
%{a: 1, b: 2}
iex(9)> d[:a]
1
iex(10)> d.a
1
iex(11)>
```

# Lambda functions

```elixir
iex(12)> f = &(&1 + 1)
#Function<6.90072148/1 in :erl_eval.expr/5>
iex(13)> f.(5)
6
iex(14)>
```

# Lazy enumerables

```elixir
iex(27)> Stream.map(1..10, &(&1))
#Stream<[enum: 1..10, funs: [#Function<42.83110278/1 in Stream.map/2>]]>
iex(28)> Stream.map(1..10, &(&1)) |> Enum.take 1
[1]
iex(29)>
```

# The pipeline operator

As just shown, the pipeline operator |>.

```elixir
iex(8)> Enum.filter 1..10, &(&1 > 2)
[3, 4, 5, 6, 7, 8, 9, 10]
iex(9)> 1..10 |> Enum.filter &(&1 > 2)
[3, 4, 5, 6, 7, 8, 9, 10]
iex(10)>
```

# A minimal gen server

Starting a minimal gen server is very easy.

```elixir
iex(1)> defmodule Test do
...(1)> use GenServer
...(1)> end
{:module, Test,
 <<70, 79, 82, 49, 0, 0, 8, 188, 66, 69, 65, 77, 69, 120, 68, 99, 0,
 0, 2, 26, 131, 104, 2, 100, 0, 14, 101, 108, 105, 120, 105, 114, 95,
 100, 111, 99, 115, 95, 118, 49, 108, 0, 0, 0, 2, 104, 2, ...>>,
 [true, true, true, true, true, true]}
iex(2)> GenServer.start Test, []
{:ok, #PID<0.62.0>}
iex(3)>
```

# Macros

Why is so easy to build the minimal gen server? Because of the macros.

```elixir
iex(1)> defmodule Test do
...(1)> defmacro imprimir(valor) do
...(1)> quote do
...(1)> IO.write unquote(valor)
...(1)> end
...(1)> end
...(1)> end
{:module, Test,
 <<70, 79, 82, 49, 0, 0, 5, 4, 66, 69, 65, 77, 69, 120, 68, 99, 0, 0,
 0, 124, 131, 104, 2, 100, 0, 14, 101, 108, 105, 120, 105, 114, 95,
 100, 111, 99, 115, 95, 118, 49, 108, 0, 0, 0, 2, 104, 2, ...>>,
{:imprimir, 1}}
iex(2)> require Test
nil
iex(3)> Test.imprimir "hola"
hola:ok
iex(4)>
```
# Calling Erlang code

```elixir
iex(5)> :io.format("%05~p~n", [1])
%051
:ok
iex(6)>
```

# Everything is an expression

Even defmodule is...

```elixir
iex(1)> p = defmodule Test, do: (def foo, do: IO.write "hola")
{:module, Test,
 <<70, 79, 82, 49, 0, 0, 4, 148, 66, 69, 65, 77, 69, 120, 68, 99, 0,
 0, 0, 92, 131, 104, 2, 100, 0, 14, 101, 108, 105, 120, 105, 114, 95,
 100, 111, 99, 115, 95, 118, 49, 108, 0, 0, 0, 2, 104, 2, ...>>,
{:foo, 0}}
iex(2)> p
{:module, Test,
 <<70, 79, 82, 49, 0, 0, 4, 148, 66, 69, 65, 77, 69, 120, 68, 99, 0,
 0, 0, 92, 131, 104, 2, 100, 0, 14, 101, 108, 105, 120, 105, 114,
 95, 100, 111, 99, 115, 95, 118, 49, 108, 0, 0, 0, 2, 104, 2, ...>>,
{:foo, 0}}
iex(3)>
```

You can save the bytecode into disk and load it later or transfer it
between machines (I saw Jose doing it in a demo ;)

```elixir
$ iex
Erlang/OTP 17 [erts-6.1] [source] [64-bit] [smp:4:4]
[async-threads:10] [hipe] [kernel-poll:false]

Interactive Elixir (0.15.2-dev) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> {_, _, b, _} = defmodule Test, do: (def foo, do: IO.write "hola")
{:module, Test,
 <<70, 79, 82, 49, 0, 0, 4, 148, 66, 69, 65, 77, 69, 120, 68, 99, 0,
 0, 0, 92, 131, 104, 2, 100, 0, 14, 101, 108, 105, 120, 105, 114, 95,
 100, 111, 99, 115, 95, 118, 49, 108, 0, 0, 0, 2, 104, 2, ...>>,
{:foo, 0}}
iex(2)> File.write! "salida.txt", b
:ok
iex(3)>
  BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded
         (v)ersion (k)ill (D)b-tables (d)istribution
$ iex
Erlang/OTP 17 [erts-6.1] [source] [64-bit] [smp:4:4]
[async-threads:10] [hipe] [kernel-poll:false]

Interactive Elixir (0.15.2-dev) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> b = File.read! "salida.txt"
<<70, 79, 82, 49, 0, 0, 4, 148, 66, 69, 65, 77, 69, 120, 68, 99, 0, 0,
0, 92, 131, 104, 2, 100, 0, 14, 101, 108, 105, 120, 105, 114, 95, 100,
111, 99, 115, 95, 118, 49, 108, 0, 0, 0, 2, 104, 2, 100, 0, 4, ...>>
iex(2)> :erlang.load_module(Test, b)
{:module, Test}
iex(3)> Test.foo
hola:ok
iex(4)>
```

I'll be extending this list as I'm finding new things out.

Have fun.
