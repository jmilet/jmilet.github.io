---
layout: post
title:  "Ready to Golang"
date:   2017-02-25 00:00:00
categories: golang
---

This is, hopefully, the first of a series of posts related to the Go programming language. This doesn't
mean that I change the topic to Go, but I want to have my findings on Go a little bit documented.

I won't detail things too much as I did with Erlang and Elixir. I'll just document the facts I find
interesting.

This is a learning exercise, so please contact and correct me if you find any mistake.

Let's Go!!!!

# The first toy program

We'll create a program that multiply some integers hold in an array by 2 and then by 4 and that
will print the result on the terminal. In order to use the Go concurrency features, each step is
going to be represented as a goroutine which will pass the current step's result to the next step,
as we'd do in a regular Elixir transformation, but this time each step is going to be running
in a different process.

```
[Goroutine main: array a[1,2,3,4,5,6,7,8] for each element in a]
        element
            |> [Goroutine: x * 2]
            |> [Goroutine: x * 4]
            |> [Goroutine: print res]
```

Speaking in Erlang terms, our program will be composed by four processes:

1. The main process
2. The x2 process
3. The x4 process
4. The logger process
<br>

The key points here are:

* Processes communicate through channels
* Processes listen to channels and the number of listeners per channel is unlimited
* Processes react to the receviced work and can place new work into other channels
* Processes terminate only when channels are closed and emptied
* Channels are typed
* Channels can be marked as: read, write or read/write

Let's review the code.

```golang
package main

import (
	"fmt"
	"sync"
)

var wg sync.WaitGroup

type pair struct {
	value  int
	result int
}

func multiplyByTwo(input <-chan int, output chan<- pair) {
	defer wg.Done()

	for value := range input {
		output <- pair{value: value, result: value * 2}
	}
	close(output)
}

func multiplyByFour(input <-chan pair, output chan<- pair) {
	defer wg.Done()

	for value := range input {
		output <- pair{value: value.value, result: value.result * 4}
	}
	close(output)
}

func logResult(input <-chan pair) {
	defer wg.Done()

	for value := range input {
		fmt.Printf("Result: %d => %d\n", value.value, value.result)
	}
}

func main() {
	// Create all three channels
	chanByTwo := make(chan int)
	chanByFour := make(chan pair)
	chanLog := make(chan pair)

	// We'll note that we'll wait for three goroutines
	wg.Add(3)

	// Create the goroutines
	go multiplyByTwo(chanByTwo, chanByFour)
	go multiplyByFour(chanByFour, chanLog)
	go logResult(chanLog)

	// Values to multiply
	values := []int{1, 2, 3, 4, 5, 6, 7, 8}

	// Insert values into the calculation pipeline
	for v := range values {
		chanByTwo <- v
	}

	// Close the production channel
	close(chanByTwo)

	// Wait for all three goroutines
	wg.Wait()
}
```
<p></p>
# Goroutines

Goroutines are just regular functions. Functions, when invoked with the **go** keyword,
run in its own execution unit. In Erlangs terms we'd say that they run in its own process.

# The Wait Group

The wait group is a counter of the number of running goroutines. It's not automatic, instead
it has to be manually mantained.

Its methods are:

* Add   => Increase the counter
* Done  => Decrease the counter
* Wait  => Wait until the counter is zero
<p></p>
# The defer keyword

The defer keyword allows to defer a cleaning operation until the end of the current
function indepently of the exit path. In this case is used to mark the goroutine termination.

# Channels

Channels are quite similiar to Erlang messages, but they are pure queues. They can be buffered
or unbuffered and can be marked as read only, write only or read and write. They are typed, a channel of ints
is not the same as a channel of strings.

Channels are created with the **make** builtin function. Objects created with **make** are reference
types, which means that they are actual values that hold a reference to the actual object. Passing
a reference type means passing the object by reference, even through a channel. I think this is the
trickiest part of the language, but it's still easy enough if you are familiar with the C language.

Channels are closed with the **close** function. A closed channel is kept consumed until all its content
is retrieved. Once a channel is empty all its listener receive a termination signal.

# for range a channel

A typical pattern in Go is to loop over a range of a channel. This way the loop is alive
until the close of the channel arrives.

# The execution

Note the correct order of the loop. We are not throwing work to a pool, we're making
an ordered execution, just spread across several processes.

```
juanmi@jmimac2 ~/go/demo1/src (master) $ go run demo1.go
Result: 0 => 0
Result: 1 => 8
Result: 2 => 16
Result: 3 => 24
Result: 4 => 32
Result: 5 => 40
Result: 6 => 48
Result: 7 => 56
juanmi@jmimac2 ~/go/demo1/src (master) $
```
<p></p>
# Conclusion

I like Go. It's minimalist and quite convinient to work with. In my experience it's quite
easy to have a pretty complex working program in just a few minutes. The queues abstraction
based on channels allows to develop complex systems quite easily.

Eventhough Go is not a functional language, quite a lot of the functional patterns still apply.
Goroutines can be closures, solutions can be modeled as data transformation chains and to create
inmutable objects is quite easy. Try it.

That's it.

Have fun.
