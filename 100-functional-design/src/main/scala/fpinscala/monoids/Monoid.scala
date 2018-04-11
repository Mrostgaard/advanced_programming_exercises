// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen
package fpinscala.monoids
import scala.language.higherKinds

trait Monoid[A] {

  def op(a1: A, a2: A): A
  def zero: A

}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  // I guess such objects could be useful if we combine them with implicits

  def listMonoid[A] = new Monoid[List[A]] {
    def op (a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  // Exercise 1

  // val intAddition =
  // val intMultiplication =
  // val booleanOr =
  // val booleanAnd =

  // Exercise 2

  // def optionMonoid[A] = ...

  def dual[A] (m :Monoid[A]) = new Monoid[A] {
    def op (a1: A, a2: A) = m.op(a2,a1)
    val zero = m.zero
  }

  // Exercise 3
  // def endoMonoid[A] =

  // Exercise 4 is solved in MonoidSpec.scala

  def concatenate[A] (as: List[A], m: Monoid[A]): A =
    as.foldLeft (m.zero) (m.op)

  // Exercise 7
  //
  // Implement a productMonoid that builds a monoid out of two monoids. Test it
  // with scala check for instance by composing an Option[Int] monoid with a
  // List[String] monoid and running through our monoid laws.

  // def productMonoid[A,B] (ma: Monoid[A]) (mb: Monoid[B]) =

}


trait Foldable[F[_]] {

  def foldRight[A,B] (as: F[A]) (z: B) (f: (A,B) => B): B
  def foldLeft[A,B] (as: F[A]) (z: B) (f: (B,A) => B): B
  def foldMap[A,B] (as: F[A]) (f: A => B) (mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  // Exercise 9 (CB 10.15)

  def toList[A] (fa: F[A]) :List[A] =
    foldMap(fa)(a=>List(a))(Monoid.listMonoid)
}

// Exercise 8 (CB 10.12 We just do Foldable[List])

object Foldable extends Foldable[List] {

  def foldRight[A,B] (as: List[A]) (b: B) (f: (A,B) => B): B =
    if(as.isEmpty) b else foldRight(as.tail)(f(as.head,b))(f)

  def foldLeft[A,B] (as: List[A]) (b: B) (f: (B,A) => B): B = 
    if(as.isEmpty) b else f(foldLeft(as.tail)(b)(f), as.head)

  def foldMap[A,B] (as: List[A]) (f: A => B) (mb: Monoid[B]): B =
    if(as.isEmpty) mb.zero else mb.op(f(as.head),foldMap(as.tail)(f)(mb))
}

// vim:cc=80:tw=80
