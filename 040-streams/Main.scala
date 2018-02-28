// Advanced Programming
// Andrzej Wąsowski, IT University of Copenhagen
//
// A script meant to be loaded into REPL (scala -i Main.scala)

import fpinscala.laziness._
import fpinscala.laziness.Stream._

// this is how we do simple interactive testing

val l1 :Stream[Int] = Empty
val l2 :Stream[Int] = empty

val l3 :Stream[Int]= cons(1, cons(2, cons (3, empty)))

println (l1.headOption)
println (l2.headOption)
println (l3.headOption)


val naturals = from(0).take(5000000).toList
println (naturals)

val to10 = to(10).toList
val to20 = to(20).toList

println(to10)
println(to20)

val naturals2 = naturals.takeWhile(_<1000000000).drop(100).take(50).toList
println(naturals2)
