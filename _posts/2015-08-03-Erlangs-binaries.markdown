---
layout: post
title:  "Erlang's binaries"
date:   2015-08-03 22:30:00
categories: binaries erlang
---

At first glance, Erlang's binaries seem quite complicated, but after
playing a little bit with them they seem not so hard.

Remember, these are just my notes and I can be wrong. In that case,
please contact and correct me in order to fix the notes.


# What is an Erlang binary?

An Erlang binary is a sequence of data where each element is a value
between 0 and 255.

# How are they structured?

They're structured in elements of 8 bits, but it's not necessary that
the final one is multiple of 8. If it is, then we say we have a
"binary", if it is not, then we say we have a "bitstring".

```
Number of bits multiple of 8       =>   binary
Number of bits non multiple of 8   =>   bitstring
```

# How they look like?

Binaries are enclosed in <<>>.

For example:

```erlang
$ erl
Erlang/OTP 18 [erts-7.0] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe]
[kernel-poll:false]

Eshell V7.0  (abort with ^G)
1> <<12, 0>>.
<<12,0>>
2>
```

The same way strings are lists whose elements are printable, binaries
can also store printable content.

```erlang
2> <<"hola">>.
<<"hola">>
3> <<65, 66>>.
<<"AB">>
4>
```

The value:size syntax allows to set the number of bits of each element.

```erlang
4> <<255:1>>.
<<1:1>>
5> <<255:2>>.
<<3:2>>
6> <<255:3>>.
<<7:3>>
7> <<255:7>>.
<<127:7>>
8> <<255:8>>.
<<"ÿ">>
9>
```

Erlang represents the data as a sequence of bytes from left to
right. The remaining part, in the case of a bitstring, is shown in the
value:size form.

```erlang
9> <<255:1, 0>>.
<<128,0:1>>
10>
```

This example shows how 255:1 takes just 1 bit from the value 255. Then
Erlang assumes that 255:1 has to be represented as a byte. After that
the 0 is represented with just the one remaining bit.

Graphically it would be something like this.

```
10000000|0
```

A four bits "bitstring".

```erlang
10> <<255:2, 0:2>>.
<<12:4>>
11> <<0, 255:2, 0:2>>.
<<0,12:4>>
12>
```

Regular Erlang pattern matching also applies to binaries.

```erlang
12> <<A:2, B:6>> = <<255>>.
<<"ÿ">>
13> A.
3
14> B.
63
15>
```

We can get the remaining part of a binary if we give Erlang a hint
about what comes next, with the "bitstring" and "binary" modifiers.

```erlang
15> <<C:2, Rest1/bitstring>> = <<255>>.
<<"ÿ">>
16> C.
3
17> Rest1.
<<63:6>>
18> <<D:8, Rest2/binary>> = <<255, 128>>.
<<255,128>>
19> D.
255
20> Rest2.
<<128>>
21> <<E, F, Rest/binary>> = <<255, 128, 64, 32>>.
<<255,128,64,32>>
22> {E, F, Rest}.
{255,128,<<"@ ">>}
23>
```

The "size" value can be a bound variable. Note the base 2 syntax.

```erlang
23> N = 5.
5
24> <<G:N/bitstring, H/bitstring>> = <<2#111111111111111111:18>>.
<<255,255,3:2>>
25> {G, H}.
{<<31:5>>,<<255,31:5>>}
26>
```

We also can use Bit Comprehensions.

```erlang
26> << <<X>> || <<X:3>> <= <<255>> >>.
<<7,7>>
27> << <<X>> || <<X:3>> <= <<205>>, X > 3 >>.
<<6>>
28>
```

Some binary related functions.

```erlang
28> BinaryFunction = term_to_binary(fun (X) -> X + 1 end).
<<131,112,0,0,2,187,1,103,57,49,11,11,201,159,65,226,96,
  121,97,18,82,151,208,0,0,0,6,0,0,...>>
29> byte_size(BinaryFunction).
701
30> Function = binary_to_term(BinaryFunction).
#Fun<erl_eval.6.54118792>
31> Function(10).
11
32>
```

Finally, a toy example which finds a token and returns the rest of the
binary.

```erlang
-module(binary_test).
-export([find/2]).

find(Key, Binary) ->
    N = byte_size(Key),

    case Binary of
        <<Key:N/binary, Rest/binary>> ->
            Rest;

        <<_B:1/binary, Rest/binary>> ->
            find(Key, Rest);

        <<>> ->
            <<>>
    end.
```

Let's test it.

```erlang
32> c(binary_test).
{ok,binary_test}
33> binary_test:find(<<"it">>, <<"Does it work?">>).
<<" work?">>
34>
```


That's it.

Have fun.
