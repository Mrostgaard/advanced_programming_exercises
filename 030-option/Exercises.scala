// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//

// AUTHOR1: matho
// AUTHOR2: mrom
// Group number:
//
// Write names and ITU email addresses of both group members that contributed to
// the solution of the exercise (in alphabetical order by family name).
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file is meant to be compiled as follows:
//
// scalac Exercises.scala
//
// or
//
// fsc Exercises.scala
//
// To run the compiled file execute "scala Tests" from a command line (or use
// another wayt to execute the Tests class from your IDE).
//
// To load the file int the REPL start the 'scala' interpreter and issue the
// command ':load Exercises.scala'. Now you can interactively experiment with
// your code.
//
// Continue solving exercises in the order presented in the PDF file. Large
// parts of the file are commented in order to make sure that the exercise
// compiles.  Uncomment and complete fragments as you proceed.  The file shall
// always compile and run after you are done with each exercise (if you do them
// in order).  Please compile and test frequently.

// Exercise  1

/* We create OrderedPoint as a trait instead of a class, so we can mix it into
 * Points (this allows to use java.awt.Point constructors without
 * reimplementing them). As constructors are not inherited, We would have to
 * reimplement them in the subclass, if classes not traits are used.  This is
 * not a problem if I mix in a trait construction time. */

 import java.awt.Point

 trait OrderedPoint extends Ordered[Point] {
   this :Point =>
   override def compare (that :Point) :Int = {
     if(x < that.x) -1
     else if(x == that.x && y < that.y) -1
     else if(x == that.x && y == that.y) 0
     else 1
  }

}

// Chapter 3

sealed trait Tree[+A]
case class Leaf[A] (value: A) extends Tree[A]
case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 2 (3.25)

  def size[A] (t :Tree[A]) :Int = t match{
    case leaf:Leaf[A] => 1
    case branch:Branch[A] => 1 + size(branch.right) + size(branch.left)
  }

  // Exercise 3 (3.26)

  def maximum (t: Tree[Int]) :Int = t match {
    case leaf:Leaf[Int] => leaf.value
    case branch:Branch[Int] => maximum(branch.right) max maximum(branch.left)
  }

  // Exercise 4 (3.28)

  def map[A,B] (t: Tree[A]) (f: A => B) : Tree[B] = t match {
    case leaf:Leaf[A] => Leaf(f(leaf.value))
    case branch:Branch[A] => Branch(map(branch.left)(f),map(branch.right)(f))
  }

  // Exercise 5 (3.29)

  def fold[A,B] (t: Tree[A]) (f: (B,B) => B) (g: A => B) :B = t match {
    case leaf:Leaf[A] => g(leaf.value)
    case branch:Branch[A] => f(fold(branch.left)(f)(g),fold(branch.right)(f)(g))
  }


  def size1[A] (t :Tree[A]) :Int = fold(t)((l:Int, r:Int) => l+r+1)((v) => 1)

  def maximum1 (t :Tree[Int]) :Int = fold(t)((l:Int, r:Int) => l max r)(v => v)
  def map1[A,B] (t: Tree[A]) (f: A => B) :Tree[B] = fold(t)((l:Tree[B], r:Tree[B]) => Branch(l,r):Tree[B])(v => Leaf(f(v)))

}

sealed trait Option[+A] {

  // Exercise 6 (4.1)
  def map[B] (f: A=>B) : Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  // Ignore the arrow in default's type below for the time being.
  // (it should work (almost) as if it was not there)

  def getOrElse[B >: A] (default: => B) :B = this match {
    case None => default
    case Some(x) => x
  }

  def flatMap[B] (f: A=>Option[B]) : Option[B] = this match {
    case None => None
    case Some(x) => f(x)
  }

  def filter (f: A => Boolean) : Option[A] = this match {
    case None => None
    case Some(x) => if(f(x)){
      Some(x)
    } else {
      None
    }
  }
}

case class Some[+A] (get: A) extends Option[A]
case object None extends Option[Nothing]

object ExercisesOption {

  // Remember that mean is implemented in Chapter 4 of the text book

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 7 (4.2)

  def variance (xs: Seq[Double]) : Option[Double] = {
    if(xs.isEmpty){None}
    else{
      val m = mean(xs)
      mean(xs.map(x => math.pow(x - m.getOrElse(0.0), 2)))
    }
  }

  // Exercise 8 (4.3)
  def map2[A,B,C] (ao: Option[A], bo: Option[B]) (f: (A,B) => C) :Option[C] = ao.flatMap(a => bo.map( b => f(a,b)))

  // Exercise 9 (4.4)

  def sequence[A] (aos: List[Option[A]]) : Option[List[A]] = aos.foldRight[Option[List[A]]](Some(Nil))((a,b) => map2(a, b)((x, xs) => x :: xs))


//def sequence[A] (aos: List[Option[A]]) : Option[List[A]] = //aos.foldRight(List[A]())((a:Option[A], b:List[A]) => b :: a.getOrElse(None) )
  // Exercise 10 (4.5)

  def traverse[A,B] (as: List[A]) (f :A => Option[B]) :Option[List[B]] = as.foldRight[Option[List[B]]](Some(Nil))((a,b) => map2(f(a), b)((x, xs) => x :: xs))



}


// Test cases for running in the compiled vesion (uncomment as you go, or paste
// them into REPL in the interactive version)

object Tests extends App {

  // Exercise 1
   val p = new java.awt.Point(0,1) with OrderedPoint
   val q = new java.awt.Point(0,2) with OrderedPoint
   assert(p < q)


  // Notice how we are using nice infix comparison on java.awt
  // objects that were implemented way before Scala existed :) (And without the
  // library implementing a suitable comparator). We did not have to recompile
  // java.awt.Point


  // Exercise 2
  assert (Tree.size (Branch(Leaf(1), Leaf(2))) == 3)
  // Exercise 3
  assert (Tree.maximum (Branch(Leaf(1), Leaf(2))) == 2)
  // Exercise 4
  val t4 = Branch(Leaf(1), Branch(Branch(Leaf(2),Leaf(3)),Leaf(4)))
  val t5 = Branch(Leaf("1"), Branch(Branch(Leaf("2"),Leaf("3")),Leaf("4")))
  assert (Tree.map (t4) (_.toString) == t5)

  // Exercise 5
  assert (Tree.size1 (Branch(Leaf(1), Leaf(2))) == 3)
  assert (Tree.maximum1 (Branch(Leaf(1), Leaf(2))) == 2)
  assert (Tree.map1 (t4) (_.toString) == t5)

  // Exercise 6
  assert (Some(1).map (x => x +1) == Some(2))
  assert (Some(41).getOrElse(42) == 41)
  assert (None.getOrElse(42) == 42)
  assert (Some(1).flatMap (x => Some(x+1)) == Some(2))
  assert ((None: Option[Int]).flatMap[Int] (x => Some(x+1)) == None)
  assert (Some(42).filter(_ == 42) == Some(42))
  assert (Some(41).filter(_ == 42) == None)
  assert ((None: Option[Int]).filter(_ == 42) == None)

  // Exercise 7
  assert (ExercisesOption.variance (List(42,42,42)) == Some(0.0))
  assert (ExercisesOption.variance (List()) == None)


  // Exercise 8
  assert (ExercisesOption.map2 (Some(42),Some(7)) (_ + _) == Some(49))
  assert (ExercisesOption.map2 (Some(42),None) (_ + _) == None)
  assert (ExercisesOption.map2 (None: Option[Int],Some(7)) (_ + _) == None)
  assert (ExercisesOption.map2 (None: Option[Int],None) (_ + _) == None)

  // Exercise 9
  assert (ExercisesOption.sequence (List(Some(1), Some(2), Some(42))) == Some(List(1,2,42)))
  assert (ExercisesOption.sequence (List(None,    Some(2), Some(42))) == None)
  assert (ExercisesOption.sequence (List(Some(1), None,    Some(42))) == None)
  assert (ExercisesOption.sequence (List(Some(1), Some(2), None    )) == None)

  // Exercise 10
  def f (n: Int) :Option[Int] = if (n%2 == 0) Some(n) else None
  assert (ExercisesOption.traverse (List(1,2,42)) (Some(_)) == Some(List(1,2,42)))
  assert (ExercisesOption.traverse (List(1,2,42)) (f) == None)

}
