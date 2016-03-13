---
layout: post
title:  "Loading confs"
date:   2015-01-10 16:20:00
categories: erlang
---

This is going to be a very short post. Just a note.

Sometimes it's useful to store some kind of configuration in a
file. In Erlang we don't have to parse the file. We just have to ask
the system to do the work.

This function...

```erlang
file:consult(Filename) -> {ok, Terms} | {error, Reason}
```

...receives the file's path and return its content as an Erlang term.

An example:

```erlang
$ cat readme.txt
{results,
 {one, 1},
 {two, 2},
 {lis, [1, 2, 3]}}.
$ erl
Erlang/OTP 17 [erts-6.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe]
[kernel-poll:false]

Eshell V6.1  (abort with ^G)
1> file:read_file("readme.txt").
{ok,<<"{results,\n {one, 1},\n {two, 2},\n {lis, [1, 2, 3]}}.\n">>}
2> {ok, [Term]} = file:consult("readme.txt").
{ok,[{results,{one,1},{two,2},{lis,[1,2,3]}}]}
3> Term.
{results,{one,1},{two,2},{lis,[1,2,3]}}
4>
```

Also related to this.

```erlang
$ erl
Erlang/OTP 17 [erts-6.1] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe]
[kernel-poll:false]

Eshell V6.1  (abort with ^G)
1> A = lists:seq(1, 10).
[1,2,3,4,5,6,7,8,9,10]
2> B = term_to_binary(A).
<<131,107,0,10,1,2,3,4,5,6,7,8,9,10>>
3> C = binary_to_term(B).
[1,2,3,4,5,6,7,8,9,10]
4>
```

Have fun.
