---
layout: post
title:  "Recursion, Folding and EUnit"
date:   2014-06-07 21:00:00
categories: erlang EUnit
---

Today, reading "Programming Erlang, Software for a Concurrent World"
(highly recommended book) I have come across an example of parsing html
using pattern matching. I thought it could be interesting to play a little
bit with it in order to exercise pattern matching and recursion-folding
techniques.

Since a few days ago, I have also wanted to practice EUnit, so I have tried to
combine the two things in just one small program. So here is the post.

# The goal

Our proof of concept will consist of a silly parser that will ident
an string dilimited by nested parents, like this (a (b b) a).

Graphically:

	(a (b b) a)

Will result in:

     a
         b
         b
     a

As said before, to accomplish this, we'll use pattern matching, recursion
with folding and unit tests. We'll also take advantage of my new discovery,
the 'escript' command, so that everything gets documented. 

Very quickly. We'll use just two files:

* A makefile
* A plain Erlang file that will have our folding function, a main function and a
test function. Usually, these elements should live in different files, but for
the sake of simplicity, we'll put them together in just one file.

Let's see the code.

{% highlight erlang %}
-module(parents).
-include_lib("eunit/include/eunit.hrl").
-export([process/1, main/1]).

-define(SPACES, "    ").


process1("(" ++ T, Level, Acc) ->
    process1(T, Level + 1, Acc);
process1(")" ++ T, Level, Acc) ->
    process1(T, Level - 1, Acc);
process1([Val|T], Level, Acc) ->
    process1(T, Level, [string:copies(?SPACES, Level) ++ [Val] | Acc]);
process1([], _Level, Acc) ->
    Acc.

process(String) ->
    lists:reverse(process1(String, -1, [])).

main(_) ->
    Result = process("(a(b(c)b)a)"),
    
    lists:foreach(fun(X) ->
			 io:format("~p~n", [X])
		  end,
		  Result).

prueba0001_test() ->
    Input = "(a(b(c)b)a)",

    [R1, R2, R3, R4, R5] = process(Input),

    ?assert(R1 =:= string:copies(?SPACES, 0) ++ "a"),
    ?assert(R2 =:= string:copies(?SPACES, 1) ++ "b"),
    ?assert(R3 =:= string:copies(?SPACES, 2) ++ "c"),
    ?assert(R4 =:= string:copies(?SPACES, 1) ++ "b"),
    ?assert(R5 =:= string:copies(?SPACES, 0) ++ "a").    
{% endhighlight %}

#
# Recursion and Folding

This is the main folding function.

{% highlight erlang %}
Process(String) ->
    lists:reverse(process1(String, -1, [])).
{% endhighlight %}

The important bit here is that it delegates the hard work
to the process/1 function which implements the pattern matching
and the folding.

It passes three parameters in:

* The input string to process
* An accumulator which values represents the nesting level
* An accumulator to store the result as it's being built

An important aspect to take into account is that the result is built
by adding elements to the __head__ of the list. This is important,
because the outcome of all this head adding is that the resulting list
ends up built in reverse order. We could add elements to the tail,
but it would be inefficient because the whole list would have to
be traversed to find the last element in each addition. On the
contrary no search is required to find the head of the list.

For example:

The input:

    [a, b, c, d]

After being processed by a folding funtion produces the output:

      [d | [c | [b | [a]]]] =:= [d, c, b, a]

That's why we return the reverse of the process1/3 function's result.
It seems that this last step is highly optimized in all functional languages
and we don't have to worry about any performance penalty for the addtional
reversing step.

The process1/3 function is interesting because it pattern matchs opening
parents, values, closing parents and the end of the input. It can be
seen how the tail and the updated accumulators are passed around.

It's also worth mentioning that this function is tail recursive. In
other words, each recursion call has no code waiting for it to
finish, so the compiler can destroy the stack created for the current
call (as it knows that we won't never return to that execution
point never again).

{% highlight erlang %}
process1("(" ++ T, Level, Acc) ->
    process1(T, Level + 1, Acc);
process1(")" ++ T, Level, Acc) ->
    process1(T, Level - 1, Acc);
process1([Val|T], Level, Acc) ->
    process1(T, Level, [string:copies(?SPACES, Level) ++ [Val] | Acc]);
process1([], _Level, Acc) ->
    Acc.
{% endhighlight %}

# Running from 'escript'

The main function is here in order to show how 'escript' works. Function
main/1 is the execution entry point to the program when we run it with
'escript'.

{% highlight erlang %}
main(_) ->
    Result = process("(a(b(c)b)a)"),
    
    lists:foreach(fun(X) ->
			  io:format("~p~n", [X])
 		  end,
		  Result).
{% endhighlight %}

# Running the tests

EUnit is quite easy to use. Nothing especial has been to be
installed in order to have it running. We've just have to include this header in
the code.

{% highlight erlang %}
-include_lib("eunit/include/eunit.hrl").
{% endhighlight %}

EUnit will recognize as a test any function which ends in "_test".

{% highlight erlang %}
prueba0001_test() ->
    Input = "(a(b(c)b)a)",

    [R1, R2, R3, R4, R5] = process(Input),

    ?assert(R1 =:= string:copies(?SPACES, 0) ++ "a"),
    ?assert(R2 =:= string:copies(?SPACES, 1) ++ "b"),
    ?assert(R3 =:= string:copies(?SPACES, 2) ++ "c"),
    ?assert(R4 =:= string:copies(?SPACES, 1) ++ "b"),
    ?assert(R5 =:= string:copies(?SPACES, 0) ++ "a").    
{% endhighlight %}

# The makefile

Just a simple makefile to clean up, build, run and test our
proof of concept.

{% highlight makefile %}
all: compile test

compile:
        erlc *.erl

test:
        erl -noshell -pa ebin -eval 'eunit:test(".",[verbose])' -s init stop

run:
        escript parents.erl

clean:
        rm *.beam
        rm *~*
{% endhighlight %}

# Our program in action

Let's see how the execution looks like.

{% highlight bash %}
$ make compile
erlc *.erl
$ make run
escript parents.erl
parents.erl:27: Warning: function prueba0001_test/0 is unused
"a"
"    b"
"        c"
"    b"
"a"
$ make test
erl -noshell -pa ebin -eval 'eunit:test(".",[verbose])' -s init stop
======================== EUnit ========================
directory "."
  parents: prueba0001_test (module 'parents')...[0.002 s] ok
  [done in 0.028 s]
=======================================================
  Test passed.
$
{% endhighlight %}

Ignore the warning. It's due to the fact that EUnit
exports the test function automatically, and we don't
export it explicitlly.

That's it.

Have fun.

