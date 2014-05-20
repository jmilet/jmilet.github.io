---
layout: post
title:  "RabbitMQ Basics"
date:   2014-05-20 21:54:00
categories: erlang rabbitmq
---

I should be writing about the second day of the feed's app, but before that
I would like to document an small prove of concept with RabbitMQ.

RabbitMQ is an amazing messaging system writen in Erlang that you
can access from any language. These are my notes about it. If you
actually want to learn RabbitMQ, please read RabbitMQ
in Action.

Clearly RabbitMQ it's going to change the way I look at some kind of
problems.

## Installation

Just go to the RabbitMQ Download page and look for the binary tar file.

For example:

{% highlight bash %}
$ tar xvfz rabbitmq-server-generic-unix-3.3.1.tar.gz
{% endhighlight %}

## Start the system up

Starting the server up actually means to start an Erlang node up.

{% highlight bash %}
$ cd rabbitmq_server-3.3.1
$ ./sbin/rabbitmq-server

              RabbitMQ 3.3.1. Copyright (C) 2007-2014 GoPivotal, Inc.
  ##  ##      Licensed under the MPL.  See http://www.rabbitmq.com/
  ##  ##
  ##########  Logs: ./sbin/../var/log/rabbitmq/rabbit@debmac.log
  ######  ##        ./sbin/../var/log/rabbitmq/rabbit@debmac-sasl.log
  ##########
              Starting broker... completed with 0 plugins.
{% endhighlight %}

Now, let's start the RabbitMQ app. Run:

{% highlight bash %}
$ ./sbin/rabbitmqctl start_app
Starting node rabbit@debmac ...
...done.
$
{% endhighlight %}

We need a producer.

{% highlight python %}
import pika, sys
import time, random


credentials = pika.PlainCredentials("guest", "guest")
conn_params = pika.ConnectionParameters("localhost",
                                        credentials = credentials)


conn_broker = pika.BlockingConnection(conn_params)
channel = conn_broker.channel()

channel.exchange_declare(exchange="hello-exchange",
                         type="direct",
                         passive=False,
                         durable=True,
                         auto_delete=False)

#msg = sys.argv[1]
limit = int(sys.argv[1])
msg_props = pika.BasicProperties()
msg_props.content_type = "text/plain"

for msg in range(limit):
   print msg
   channel.basic_publish(body=str(msg) + " hola" * 10,
                         exchange="hello-exchange",
                         properties=msg_props,
                         routing_key="hola")
   #time.sleep(random.uniform(0, .2))
{% endhighlight %}

And some consumers...

{% highlight python %}
import pika
import time, random

credentials = pika.PlainCredentials("guest", "guest")
conn_params = pika.ConnectionParameters("localhost",
                                        credentials = credentials)


conn_broker = pika.BlockingConnection(conn_params)
channel = conn_broker.channel()
channel.basic_qos(prefetch_count=1)

channel.exchange_declare(exchange="hello-exchange",
                         type="direct",
                         passive=False,
                         durable=True,
                         auto_delete=False)

channel.queue_declare(queue="hello-queue", auto_delete=True)
channel.queue_bind(queue="hello-queue",
                   exchange="hello-exchange",
                   routing_key="hola")

def msg_consumer(channel, method, header, body):
    if body == "quit":
        channel.basic_cancel(consumer_tag="hello-consumer")
        channel.stop_consuming()
    else:
        print body
        time.sleep(1)
        channel.basic_ack(delivery_tag=method.delivery_tag)


channel.basic_consume(msg_consumer,
                      queue="hello-queue",
                      consumer_tag="hello-consumer")
channel.start_consuming()
{% endhighlight %}

In Java...

{% highlight java %}
import java.io.IOException;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.QueueingConsumer;

public class ConsumerTest {

    private static final String EXCHANGE_NAME = "hello-exchange";

    public static void main(String[] argv)
        throws java.io.IOException,
               java.lang.InterruptedException {

        ConnectionFactory factory = new ConnectionFactory();
        factory.setHost("localhost");
        factory.setUsername("guest");
        factory.setPassword("guest");
        Connection connection = factory.newConnection();
        Channel channel = connection.createChannel();

        channel.exchangeDeclare(EXCHANGE_NAME, "direct", true /* durable */ );
        String queueName = "hello-queue";
        channel.queueDeclare(queueName, false /* durable */ , false /* exclusive */ , true /* autodelete */, null /* arguments */);
        channel.queueBind(queueName, EXCHANGE_NAME, "hola");
        channel.basicQos(/* prefetch */ 1);

        System.out.println(" [*] Waiting for messages. To exit press CTRL+C");

        QueueingConsumer consumer = new QueueingConsumer(channel);
        channel.basicConsume(queueName, false /* autoAck */, consumer);

        while (true) {
            QueueingConsumer.Delivery delivery = consumer.nextDelivery();
            String message = new String(delivery.getBody());
            Thread.sleep(1000);
            channel.basicAck(delivery.getEnvelope().getDeliveryTag(), false /* multiple */ );

            System.out.println(" [x] Received '" + message + "'");
        }
    }
}
{% endhighlight %}

Let's run the consumers:

{% highlight bash %}
$ python consumer.py
...
$ javac -cp $HOME/rabbitmq-java-client-bin-3.3.1/rabbitmq-client.jar:. ConsumerTest.java
$ java -cp $HOME/rabbitmq-java-client-bin-3.3.1/rabbitmq-client.jar:. ConsumerTest
{% endhighlight %}

And the producer... Make sure all sessions are visible:

{% highlight bash %}
$ python producer.py 20
{% endhighlight %}

The results are... for the Python consumer:

{% highlight bash %}
$ python consumer.py
0 hola hola hola hola hola hola hola hola hola hola
2 hola hola hola hola hola hola hola hola hola hola
4 hola hola hola hola hola hola hola hola hola hola
6 hola hola hola hola hola hola hola hola hola hola
8 hola hola hola hola hola hola hola hola hola hola
10 hola hola hola hola hola hola hola hola hola hola
12 hola hola hola hola hola hola hola hola hola hola
14 hola hola hola hola hola hola hola hola hola hola
16 hola hola hola hola hola hola hola hola hola hola
18 hola hola hola hola hola hola hola hola hola hola
{% endhighlight %}

For the Java one:

{% highlight bash %}
$ java -cp $HOME/rabbitmq-java-client-bin-3.3.1/rabbitmq-client.jar:. ConsumerTest
 [*] Waiting for messages. To exit press CTRL+C
 [x] Received '1 hola hola hola hola hola hola hola hola hola hola'
 [x] Received '3 hola hola hola hola hola hola hola hola hola hola'
 [x] Received '5 hola hola hola hola hola hola hola hola hola hola'
 [x] Received '7 hola hola hola hola hola hola hola hola hola hola'
 [x] Received '9 hola hola hola hola hola hola hola hola hola hola'
 [x] Received '11 hola hola hola hola hola hola hola hola hola hola'
 [x] Received '13 hola hola hola hola hola hola hola hola hola hola'
 [x] Received '15 hola hola hola hola hola hola hola hola hola hola'
 [x] Received '17 hola hola hola hola hola hola hola hola hola hola'
 [x] Received '19 hola hola hola hola hola hola hola hola hola hola'
{% endhighlight %}

Some useful commands:

{% highlight bash %}
$ ./sbin/rabbitmqctl list_queues
Listing queues ...
hello-queue     0
...done.
$ ./sbin/rabbitmqctl list_exchanges
Listing exchanges ...
        direct
amq.direct      direct
amq.fanout      fanout
amq.headers     headers
amq.match       headers
amq.rabbitmq.log        topic
amq.rabbitmq.trace      topic
amq.topic       topic
hello-exchange  direct
...done.
$ ./sbin/rabbitmqctl stop_app
Stopping node rabbit@debmac ...
...done.
$ ./sbin/rabbitmqctl reset
Resetting node rabbit@debmac ...
...done.
$ ./sbin/rabbitmqctl start_app
Starting node rabbit@debmac ...
...done.
$
{% endhighlight %}

Final thoughts. Exchanges are the tool that allows that a producer
can be decoupled from the queues, while queues are connected to
exchanges.

{% highlight bash %}
                          +---> Queue 1 --> Consumer
[Producer] --> Exchange --+---> Queue 2 --> Consumer
                          +---> ...
                          +---> Queue N --> Consumer
{% endhighlight %}

ACK's can be automatic or manual, if manual we use channel.basic_ack in
order to let the server know that we are done with the processing of that
element.

Hopefully in the next few days I will have some spare time to get
into more detail with an Erlang example.

Have fun.

