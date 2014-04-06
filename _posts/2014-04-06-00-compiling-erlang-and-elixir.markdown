---
layout: post
title:  "Installing Erlang and Elixir from the source"
date:   2014-04-06 12:12:00
categories: erlang elixir programming
---

This is a simple guide to download and install Erlang and Elixir in your computer. I won't go
into too much detail because if you're reading this it's clear you're familiar with the UNIX shell.

**These instructions are here in the hope that they will be useful, but without any warranty.**

It's assumed that you're using a Debian box, if you use another distro you will have to find out
how to install the dependencies. For Ubuntu it should be the same as for Debian.

{% highlight bash %}
$ uname -vr
3.2.0-4-686-pae #1 SMP Debian 3.2.32-1
{% endhighlight %}

Some development packages have to be installed:

{% highlight bash %}
# Install dependencies.
$ sudo apt-get install libncurses5-dev libssl-dev autoconf
{% endhighlight %}

Copy & paste this into a file and chmod +x it or execute each part step by step if you prefer.

{% highlight bash %}
#!/bin/bash

# This script is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY.

# Go home.
cd $HOME

# Make the compilation directories.
mkdir erlang_elixir
mkdir erlang_elixir/erlang
mkdir erlang_elixir/elixir

# Download and compile Erlang.
cd $HOME/erlang_elixir/erlang
wget http://www.erlang.org/download/otp_src_17.0-rc2.tar.gz
tar xvfz otp_src_17.0-rc2.tar.gz
cd $HOME/erlang_elixir/erlang/otp_src_17.0-rc2
./configure --prefix=$HOME/erlelx/erlang && make && make install
export PATH=$PATH:$HOME/erlelx/erlang/bin

# Download and compile Elixir.
cd $HOME/erlang_elixir/elixir
git clone https://github.com/elixir-lang/elixir.git
cd $HOME/erlang_elixir/elixir/elixir
make clean test
cd $HOME/erlelx
cp -a $HOME/erlang_elixir/elixir/elixir .
{% endhighlight %}

To run it as an script.

{% highlight bash %}
$ chmod u+x my_elixir_intall_script.sh
$ ./my_elixir_intall_script.sh
{% endhighlight %}

It just make a directory named $HOME/erlang_elixir where the compilation is done and another one named
$HOME/erlelx where the actual binaries will be placed.

Update the PATH variable for this session and include it in your .bashrc for future ones.

{% highlight bash %}
export PATH=$PATH:$HOME/erlelx/erlang/bin:$HOME/erlelx/elixir/bin
{% endhighlight %}

By the time of writing this Erlang/OTP 17 is still beta, when it's the stable realease I will
update the instructions to git clone the Erlang sources.

You can now run Erlang with erl:

{% highlight erlang %}
$ erl
Erlang/OTP 17 [RELEASE CANDIDATE 2] [erts-6.0] [source] [smp:2:2] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.0  (abort with ^G)
1> io:format("Hello World!~n").
Hello World!
ok
2>
{% endhighlight %}

And elixir with iex:

{% highlight erlang %}
$ iex
Erlang/OTP 17 [RELEASE CANDIDATE 2] [erts-6.0] [source] [smp:2:2] [async-threads:10] [hipe] [kernel-poll:false]

Interactive Elixir (0.13.0-dev) - press Ctrl+C to exit (type h() ENTER for help)
iex(1)> IO.puts "Hello World!"
Hello World!
:ok
iex(2)>
{% endhighlight %}

Enjoy!

