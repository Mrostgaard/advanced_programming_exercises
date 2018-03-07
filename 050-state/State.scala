package stateful
trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  // Exercise 1 (CB 6.1)

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
      //I make a new function call if this happens, because having it point to a constant int, would make that int twice as likely to be randomly generated.
      case (i,s) if(i == Int.MinValue) => nonNegativeInt(rng) 
      case (i,s) if(i < 0) => (i * -1, s)
      case (i,s) => (i,s)
  }
  

  // Exercise 2 (CB 6.2)

  def double(rng: RNG): (Double, RNG) = { 
    val t = this.nonNegativeInt(rng)
    val d = t._1.toDouble / Int.MaxValue.toDouble
    (d,t._2)
  }
  // Exercise 3 (CB 6.3)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val it = nonNegativeInt(rng)
    val dt = double(rng)
    ((it._1,dt._1),it._2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val it = nonNegativeInt(rng)
    val dt = double(rng)
    ((dt._1,it._1),it._2)
  }
  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  // Exercise 4 (CB 6.4)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if(count < 0)
      (List[Int](),rng)
    else {
      val t1 = nonNegativeInt(rng)
      val t2 = ints(count-1)(t1._2)
      (t1._1 :: t2._1, t2._2)
    }
  }

  // There is something terribly repetitive about passing the RNG along
  // every time. What could we do to eliminate some of this duplication
  // of effort?

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // Exercise 5 (CB 6.5)

  val _double2: Rand[Double] = map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)

  // Exercise 6 (CB 6.6)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rnga) = ra(rng)
      val (b, rngb) = rb(rnga)
      (f(a,b),rngb)
    }

  // this is given in the book

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // Exercise 7 (6.7)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case x :: xs => {
        rng =>{
          val (tail,rng2) = sequence(xs)(rng)
          (x(rng)._1 :: tail, rng2)
        }
      }
    case Nil => {
        rng => (List[A](), rng)
      }
    }

  def _ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(map(nonNegativeInt)(i => i)))

  // Exercise 8 (6.8)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng =>{
      val(a,rnga) = f(rng)
      g(a)(rnga)
    }

  // def nonNegativeLessThan(n: Int): Rand[Int] = { ...

}

import State._

case class State[S, +A](run: S => (A, S)) {

  // Exercise 9 (6.10)

  def map[B](f: A => B): State[S, B] = {
    def run2 = (s:S) =>{
      val (a, s2) = run(s)
      (f(a),s2)
    }
    State[S, B](run2)
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    def run2 = (s:S) => {
      val(a,s2) = run(s)
      val(b,s3) = sb.run(s2)
      (f(a, b),s3)
      }
    State[S, C](run2)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a,s2) = run(s)
    f(a).run(s2)
  })

}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // Exercise 9 (6.10) continued

  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] = 
    if(sas.isEmpty) State( s => {
        (List[A](),s)
      })
    else{
      State( s => {
        val(a, s2) = sas.head.run(s)
        val(tail, s3) = sequence(sas.tail).run(s2)
        (a :: tail, s3)
      })
    }
  //
  // This is given in the book:

  // def modify[S](f: S => S): State[S, Unit] = for {
  //   s <- get // Gets the current state and assigns it to `s`.
  //   _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  // } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))


  def random_int :Rand[Int] =  State (_.nextInt)

  // Exercise 10

  def state2stream[S,A] (s :State[S,A]) (seed :S) :Stream[A] ={
    val(a,s2) = s.run(seed)
    Stream.cons(a, state2stream(s)(s2))
  }

  // Exercise 11

  val random_integers = state2stream(State[RNG,Int](rng => rng.nextInt))(RNG.Simple(0))

}


// vim:cc=80:foldmethod=indent:foldenable
