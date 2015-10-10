---
layout: post
title:  "Erlang/Elixir's remote files"
date:   2015-10-11 00:00:00
categories: remote distributed files elixir erlang
---

Lately I've been working with files in just one machine. While working
on it I had the doubt whether files could be shared between
nodes. Files have pids so... they must be processes so... they should
be shared between nodes... Let's test it.

# The test

As asual the test is a very simple one. Just two different physical
machines and a file that will be accessed by both of them.

The first machine is a nice Raspberry Pi running FreeBSD.

```bash
$ uname -a
FreeBSD raspberry-pi 10.1-RELEASE FreeBSD 10.1-RELEASE #0 r274401: Wed Nov 12
04:42:19 UTC 2014     root@releng1.nyi.freebsd.org:/usr/obj/arm.armv6/usr/src/
sys/RPI-B  arm
$

```

The second one a regular Mac OS X.

```bash
$ uname -a
Darwin jmimac2.local 15.0.0 Darwin Kernel Version 15.0.0: Wed Aug 26 16:57:32 PDT
2015; root:xnu-3247.1.106~1/RELEASE_X86_64 x86_64
$
```

Let's start a node in the Mac.

```bash
$ iex --name mac@192.168.1.37 --cookie abc
Erlang/OTP 18 [erts-7.0] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe]
[kernel-poll:false]

Interactive Elixir (1.1.0-rc.0) - press Ctrl+C to exit (type h() ENTER for help)
iex(mac@192.168.1.37)1>
```

And we'll do the same in the Raspberry Pi.

```bash
$ iex --name raspberry@192.168.1.38 --cookie abc
Erlang/OTP 18 [erts-7.0] [source] [async-threads:10] [kernel-poll:false]

Interactive Elixir (1.1.0-dev) - press Ctrl+C to exit (type h() ENTER for help)
iex(raspberry@192.168.1.38)1>
```

Ok, let's make the two machines be aware of each other.

```bash
iex(mac@192.168.1.37)1> :net_kernel.connect :"raspberry@192.168.1.38"
true
iex(mac@192.168.1.37)2> :erlang.nodes()
[:"raspberry@192.168.1.38"]
iex(mac@192.168.1.37)3>
```

Still in the Mac, let's open a file in :write mode.

```bash
iex(mac@192.168.1.37)3> f = File.open! "fichero.txt", [:write, :utf8]
#PID<0.71.0>
iex(mac@192.168.1.37)4>
```

We see that the file actually is a process. Let's make its pid travel
to the Raspberry.

In the Raspberry we globally register the shell's pid so it can be
accesed from the remote node.

```bash
iex(raspberry@192.168.1.38)1> :global.register_name :la_shell, self()
:yes
iex(raspberry@192.168.1.38)2>
```

From the Mac we send the file to the Raspberry's shell.

```bash
iex(mac@192.168.1.37)4> send :global.whereis_name(:la_shell), f
#PID<0.71.0>
iex(mac@192.168.1.37)5>
```

Again in the Raspberry we catch the file. Note the local pid in the Mac
and the remote one in the Raspberry.

```bash
iex(raspberry@192.168.1.38)2> f = receive do
...(raspberry@192.168.1.38)2> f -> f
...(raspberry@192.168.1.38)2> end
#PID<7987.71.0>
iex(raspberry@192.168.1.38)3> f
#PID<7987.71.0>
iex(raspberry@192.168.1.38)4>
```

From the Raspberry let's write some text into the file (which is owned by the Mac) and
then close the file.

```bash
iex(raspberry@192.168.1.38)4> IO.write f, "hola que tal!!!"
:ok
iex(raspberry@192.168.1.38)5> File.close f
:ok
iex(raspberry@192.168.1.38)6>
```

Let's come back to the Mac to read the file.

```bash
iex(mac@192.168.1.37)5> File.read "fichero.txt"
{:ok, "hola que tal!!!"}
iex(mac@192.168.1.37)6>
```

Nice.

After seeing that files are processes and stress them quite a bit I'd
say that files follow processes's semantics when it comes to queuing
I/O operations. That means that the same file can be shared between
different processes and its read/write operations will be executed in
order. The file's queue would garantee that.

Please, if you can confirm or correct the last statement I'd love
to hear from you (see below how to contact me). Thanks!!!

That's it.

Have fun.
