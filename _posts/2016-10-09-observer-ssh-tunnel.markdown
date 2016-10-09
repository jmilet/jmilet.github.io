---
layout: post
title:  "Observer ssh tunnel"
date:   2016-10-09 00:00:00
categories: elixir erlang observer ssh
---

This post is just to document an script that opens an ssh tunnel to connect to
a remote Erlang VM via Observer.

First, I want to thank to my friend [Rubén Caro] [1] for sharing all
his knowledge when we worked together on this. You must check his github repos.

This is the script. You can pass it three parameters.

* The remote node name.
* The remote user name.
* The remote server.

For convenience all three parameters are given default values.

It assumes a *node@127.0.0.1* name convention for the nodes.

```bash
#!/bin/bash

EPMD=/home/jmimora/erlang/bin/epmd

# Default values.
default_node="uno"
default_user="jmimora"
default_server="192.168.1.46"


function get_params()
{
    case $# in
	0)
	    node=$default_node
	    user=$default_user
	    server=$default_server
	    ;;
	3)
	    # OK, all parameters have been given.
	    ;;
	*)
	    echo "./iex_tunnuel.sh $default_node $default_user $default_server"
	    exit
	    ;;
    esac

    echo "node: $node"
    echo "user: $user"
    echo "server: $server"

}

function get_ports()
{
    while read line
    do
	if [ "$port1" = "" ]
	then
	    port1=`echo $line | grep running | cut -d ' ' -f 7`
	fi

	if [ "$port2" = "" ]
	then
	    port2=`echo $line | grep $node | cut -d ' ' -f 5`
	fi
    done < <(ssh $user@$server $EPMD -names)
}

function make_tunnel()
{
    echo $port1, $port2

    ssh -N -L $port1:$server:$port1 -L $port2:$server:$port2 $user@$server
}

function main()
{
    # Parameters.
    node=$1
    user=$2
    server=$3

    get_params $1 $2 $3
    get_ports
    make_tunnel
}

main $1 $2 $3
```

Let's see it working.

The remote node.

```Elixir
$ iex --name uno@127.0.0.1 --cookie abc
Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:4:4] [async-threads:10]
[hipe] [kernel-poll:false] [dtrace]

Interactive Elixir (1.2.5) - press Ctrl+C to exit (type h() ENTER for help)
iex(uno@127.0.0.1)1>
```

The tunnel.

```bash
$ ./iex_tunnel.sh uno jmimora 192.168.1.46
node: uno
user: jmimora
server: 192.168.1.46
4369, 50864
```

In the local iex we will ping the remote VM.

```Elixir
$ iex --name dos@127.0.0.1 --cookie abc
Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:4:4] [async-threads:10]
[hipe] [kernel-poll:false] [dtrace]

Interactive Elixir (1.2.5) - press Ctrl+C to exit (type h() ENTER for help)
iex(dos@127.0.0.1)1> :net_adm.ping :'uno@127.0.0.1'
:pong
iex(dos@127.0.0.1)2>
```

Now you can *:start.observer* and select your remote node in the *nodes* menu.

You will have to type the remote password twice in case you
don't use ssh keys.

That's it.

Have fun.

[1]: https://github.com/rubencaro "Rubén Caro"
