// Advanced Programming
// Andrzej Wasowski, IT University of Copenhagen

package fpinscala.laziness
import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

// If you comment out all the import lines below, then you test the Scala
// Standard Library implementation of Streams. Interestingly, the standard
// library streams are stricter than those from the book, so some laziness tests
// fail on them :)

import stream00._    // uncomment to test the book solution
// import stream01._ // uncomment to test the broken headOption implementation
// import stream02._ // uncomment to test another version that breaks headOption

class StreamSpecMathoMrom extends FlatSpec with Checkers {

  import Stream._

  behavior of "headOption"

  // a scenario test:

  it should "return None on an empty Stream (01)" in {
    assert(empty.headOption == None)
  }

  // An example generator of random finite non-empty streams
  def list2stream[A] (la :List[A]): Stream[A] = la.foldRight (empty[A]) (cons[A](_,_))

  // In ScalaTest we use the check method to switch to ScalaCheck's internal DSL
  def genNonEmptyStream[A] (implicit arbA :Arbitrary[A]) :Gen[Stream[A]] =
    for { la <- arbitrary[List[A]] suchThat (_.nonEmpty)}
    yield list2stream (la)
  
  def arbFail[A](failureMessage:String) = throw new IllegalStateException(failureMessage)

  def genFailByNStream[A] (n:Int, a:A, f: A => A) : Stream[A] =
    if(n <= 0) throw new IllegalStateException("N+1 was evaluated")
    else cons(f(a), genFailByNStream(n-1,f(a),f))
  
  lazy val errorStream = {
    cons(throw new IllegalStateException("Head was evaluated"),
      throw new IllegalStateException("Tail was evaluated")) 
  }

  lazy val natural = {
    def n(i: Int) : Stream[Int] = cons(i, n(i+1))
    n(0)
  }
  def from(i: Int) : Stream[Int] = cons(i, from(i+1))

  // a property test:

  it should "return the head of the stream packaged in Some (02)" in check {
    // the implict makes the generator available in the context
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    ("singleton" |:
      Prop.forAll { (n :Int) => cons (n,empty).headOption == Some (n) } ) &&
    ("random" |:
      Prop.forAll { (s :Stream[Int]) => s.headOption != None } )

  }
  //Since the stream of natural numbers is infinite, and recursively defined, this test should break
  //if the tail is forced to be calculated.
  it should "return the head of the stream, without evaluating the whole stream" in {
    assert(cons(0,arbFail[Int]("Evaluated tail")).headOption == Some(0))
  }

  behavior of "take"

  it should "return empty stream, if taking 0 elements" in check{
    ("infinite stream" |:
      Prop.forAll { (n :Int) => from(n).take(0).take(0).toList.isEmpty == true}) &&
   ("singleton" |:
      Prop.forAll { (n :Int) => cons(n,empty).take(0).take(0).toList.isEmpty == true}) &&
    ("empty stream" |:
      Prop.forAll { (n :Int) => Stream[Int]().take(0).take(0).toList.isEmpty == true})
  }
  
  //If head or tail were to be forced, the errorStream will throw an exception.
  it should "not evaluate head of stream" in {
    cons (arbFail[Int]("Forced head"),from(0)).take(2)
  }
  it should "not evaluate tail of stream" in {
    assert(cons (1,arbFail[Int]("Forced tail")).take(1).headOption == Some(1))
  } 

  it should "not force evaluation of element n+1, when taking n elements" in check {
    ("singleton" |:
      Prop.forAll { (n :Int) => cons(n,arbFail[Int]("Evaluated tail")).take(1).headOption == Some(n)}) && 
    ("infinite stream" |:
      //We limit n, as forcing a list of too many elements might not be possible, but is outside the scope of the test
      Prop.forAll { (n :Int) =>
        from(0).take(Math.abs(n%1000)).append(cons(Math.abs(n%1000)+1, arbFail[Int]("Evaluated tail"))).take(Math.abs(n%1000)).toList.size == Math.abs(n%1000)
    })
  }
  
  it should "should conform to " in check{
     // the implict makes the generator available in the context
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    ("infinite stream" |:
      Prop.forAll { (n :Int) =>{
        val i = n/2 + Math.abs(n/2)
        val s = from(0)
        s.take(i).take(i) == s.take(i) }} ) &&
    ("singleton" |:
      Prop.forAll { (n :Int) =>{
      //val i = n/2 + Math.abs(n/2)
        val s = cons(1, empty);
        s.take(1).take(1) == s.take(1) }} ) &&
    ("random" |:
      Prop.forAll { (s :Stream[Int]) => s.take(10).take(10) == s.take(10) } )
  }
}
