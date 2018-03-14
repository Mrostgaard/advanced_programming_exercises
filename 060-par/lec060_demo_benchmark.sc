import java.util.concurrent
import java.util.concurrent._

println("benchmark ")

def time[A](a: => A) = {
  val now = System.nanoTime
  val result = a
  val micros = (System.nanoTime - now) / 1000
  println("%d microseconds".format(micros))
  result
}

type Par[A] = ExecutorService => Future[A]

object Par {
  def unit[A](a:A):Par[A] = (es:ExecutorService)=> UnitFuture(a)

  private case class UnitFuture[A] (get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  def map2[A,B,C] (a:Par[A],b:Par[B])(f:(A,B)=>C):Par[C] =
    (es:concurrent.ExecutorService) =>{
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get,bf.get))
  }

  def fork[A](a: => Par[A]) : Par[A] =
    es=>es.submit(new Callable[A] {
      override def call = a(es).get
    })
}


def sum_seq(ints: IndexedSeq[Int]): Int = {if (ints.size <= 1)
  ints.headOption getOrElse 0 else {
  val (l, r) = ints.splitAt(ints.length / 2)
  sum_seq(l) + sum_seq(r)
}}

def sum_par(ints: IndexedSeq[Int]): Par[Int] =
  if (ints.size <= 1)
    Par.unit(ints.headOption getOrElse 0)
  else {
    val (l, r) = ints.splitAt(ints.length / 2)
    Par.map2(Par.fork(Par.unit(sum_seq(l))),Par.fork(Par.unit(sum_seq(r))))(_+ _)}

println("result with par is here: ")

//val myList=20000000 to 50000000
val myList=200000 to 500000

time {
  val res_par = sum_par(myList)
  val es = Executors.newWorkStealingPool()
  res_par(es).get
}

println("result with seq is here: ")

  time{sum_seq(myList)}
