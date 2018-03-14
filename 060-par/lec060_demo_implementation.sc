import java.util.concurrent
import java.util.concurrent._

println("exploring par[A]")


type Par[A] = ExecutorService => Future[A]

//def run[A](s: ExecutorService)(a:Par[A]) = a(s)

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

def sum(ints: IndexedSeq[Int]): Par[Int] =
  if (ints.size <= 1)
    Par.unit(ints.headOption getOrElse 0)
  else {
    val (l, r) = ints.splitAt(ints.length / 2)
    Par.map2(Par.fork(sum(l)),Par.fork(sum(r))) (_+ _)
  }
println("result is here: ")
val res=sum(1 to 40 )
val es=Executors.newWorkStealingPool()
res(es).get