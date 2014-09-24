---
layout: post
title:  "Elixir Macros"
date:   2014-09-24 10:00:00
categories: erlang elixir elisp
---

When I started playing with Elixir's macros some days ago I knew they
were inspired by the Lisp's ones. I always wanted to look at the
latter, but I found them such an advanced topic that I always ended
giving up.

But this time I did it. I jumped into my scratch Emacs buffer and
began to apply all I learned about Elixir macros to Emacs Lisp. These
are my notes explaining how both languages compare in this regard.

As always remember... I'm still learning...

# Quote

Quote allows to transform code into its internal representation more
formally called
[Abstract Syntax Tree](http://en.wikipedia.org/wiki/Abstract_syntax_tree).

Note the difference between executing the code and getting its
internal representation.

In Emacs Lisp:

```Lisp
(+ 1 1)
2

'(+ 1 1)
(+ 1 1)
```

In Elixir:

```Elixir
iex(1)> 1 + 1
2
iex(2)> quote do: 1 + 1
{:+, [context: Elixir, import: Kernel], [1, 1]}
iex(3)>
```
<p>
# Unquote and Backquote

Unquote and backquote (in Emacs Lisp) allow to inject some actual code
inside the internal code's representation.

Let's see it.

In Lisp:

```Lisp
(setq x 20)
20

`(+ 1 ,x)
(+ 1 20)

(eval `(+ 1 ,x))
21
```

In Elixir:

```Elixir
iex(1)> x = 20
20
iex(2)> quote do: 1 + x
{:+, [context: Elixir, import: Kernel], [1, {:x, [], Elixir}]}
iex(3)> Code.eval_quoted quote do: 1 + unquote(x)
{21, []}
iex(4)>
```

In this example we firstly set *x* to 20. Afterwards, quoting and
later unquoting *x*, we get the internal code representation of 1 +
20, being 20 the expansion of *x*. Note the comma before the *x*. It's
not the same \`(+ 1 x) and \`(+ 1 ,x). The comma before the *x*
expands *x* to its value.

Finally, the internal code representation is evaluated in both cases.

# Macros

And the final part, macros. They behave as regular functions but work
slightly different. When the system finds a macro call it inlines the
resulting internal code representation of quoting the code and
unquoting the parameters.  The big win is that parameters can be any
kind of code allowing this to develop new language constructs. The
classical exemple is the *unless* language keyword.

In Lisp.

```Lisp
(defmacro unless(expr code)
  `(if (not ,expr)
       ,code))
unless

(unless (> 1 2) :ok)
:ok

(unless (< 1 2) :ok)
nil
```

In Elixir:

```Elixir
iex(1)> defmodule MacroTest do
...(1)>    defmacro unless(expr, code) do
...(1)>       quote do
...(1)>          if not unquote(expr) do
...(1)>             unquote(code)
...(1)>          end
...(1)>       end
...(1)>    end
...(1)> end
{:module, MacroTest,
 <<70, 79, 82, 49, 0, 0, 5, 80, 66, 69, 65, 77, 69, 120, 68, 99, 0, 0,
 0, 137, 131, 104, 2, 100, 0, 14, 101, 108, 105, 120, 105, 114, 95,
 100, 111, 99, 115, 95, 118, 49, 108, 0, 0, 0, 2, 104, 2, ...>>,
 {:unless, 2}}
iex(2)> require MacroTest
nil
iex(3)> MacroTest.unless 1 > 2, do: :ok
[do: :ok]
iex(4)> MacroTest.unless 3 > 2, do: :ok
nil
iex(5)>
```

Note that *code* took the value [do: :ok] so that was returned by its
unquoting inside then quoting.  Let's do it nicer using Keyword Lists
in order to grab the actual code in the *code* var.

```Elixir
iex(1)> defmodule MacroTest do
...(1)>    defmacro unless(expr, do: code) do
...(1)>       quote do
...(1)>          if not unquote(expr) do
...(1)>             unquote(code)
...(1)>          end
...(1)>       end
...(1)>    end
...(1)> end
{:module, MacroTest,
 <<70, 79, 82, 49, 0, 0, 5, 112, 66, 69, 65, 77, 69, 120, 68, 99, 0,
 0, 0, 141, 131, 104, 2, 100, 0, 14, 101, 108, 105, 120, 105, 114, 95,
 100, 111, 99, 115, 95, 118, 49, 108, 0, 0, 0, 2, 104, 2, ...>>,
 {:unless, 2}}
iex(2)> require MacroTest
nil
iex(3)> unless 1 > 2 do
...(3)>    :ok
...(3)> end
:ok
iex(4)> unless 3 > 2 do
...(4)>    :ok
...(4)> end
nil
iex(5)>
```

As can be seen macros are a powerful tool that easily allows to extend
the language. Both languages implement macros in a very similar way
giving access to the inner code representation and allowing the
programmer to alter them.

Macros are an open door to
[Domain Specific Languages](http://en.wikipedia.org/wiki/Domain-specific_language),
something that Elixir will excel at very soon.

Have fun.
