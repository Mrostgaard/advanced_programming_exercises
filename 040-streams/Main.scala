// Advanced Programming
// Andrzej Wąsowski, IT University of Copenhagen
//
// A script meant to be loaded into REPL (scala -i Main.scala)

import fpinscala.laziness._
import fpinscala.laziness.Stream._

// this is how we do simple interactive testing

//val l1 :Stream[Int] = Empty
//val l2 :Stream[Int] = empty

//val l3 :Stream[Int]= cons(1, cons(2, cons (3, empty)))

//println (l1.headOption)
//println (l2.headOption)
//println (l3.headOption)


val naturals = from(0)
//println (naturals.take(500).toList)

//val to10 = to(10).toList
//val to20 = to(20).toList

//println(to10)
//println(to20)

//val naturals2 = naturals.takeWhile(_<1000000000).drop(100).take(50).toList
//println(naturals2)
//Because laizily evaluation will just drop the first 100 and then take the next 50 (101 -> 150) and turn that into the list

//assert(naturals.forAll(_ < 0))
//assert(naturals.forAll(_ >= 0))

//Exercise 6
val naturals2 = naturals.takeWhileFold(_<1000000000).drop(100).take(50).toList
val naturals3 = naturals.takeWhileUnfold(_<1000000000).drop(100).take(50).toList
println(naturals2)
println(naturals3)

//Exercise 7
/*
val headOptionNone = naturals.take(0).headOption()
assert(headOptionNone == None)
val headOption1 = naturals.take(10).headOption()
assert(headOption1 == Some(0))
val headOption2 = naturals.drop(5).take(10).headOption()
assert(headOption1 == Some(6))
*/
//Exercise 8
/*
 */
//1
println(naturals.map(_*2).drop(30).take(50).toList)
println(naturals.mapUnfold(_*2).drop(30).take(50).toList)
println(naturals.mapUnfold(_*2).drop(30).takeUnfold(50).toList)
//2
/*
println(naturals.drop(42).filter(_%2 == 0).take(30).toList)
//3
println(naturals.append(naturals))
println(naturals.take(10).append(naturals).take(20).toList)
//4
println(naturals.flatMap(to _).take(100).toList)
println(naturals.flatMap(x => from(x)).take(100).toList)
*/
//Exercise 9
/*
In a list we would filter list first and then make the head of the list a headOption, in a stream we would look at the first element to pass through the filter and then make that a headOption.

*/

//Exercise 10
val fibz = fibs.take(10).toList
println(fibz)
println(fibsUnfold.take(10).toList)

//Exervise 11
assert(from(1).take(100000000).drop(41).take(10).toList == fromUnfold(1).take(100000000).drop(41).take(10).toList)
println(fromUnfold(1).take(10).toList)
