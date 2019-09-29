import java.util.concurrent._


object FP_PureParallelism extends App {

  type Par[+A] = ExecutorService => Future[A]

  implicit class RichPar[A](a: Par[A]) {
    def map2[B,C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(a,b)(f)
    def map[B](f: A => B): Par[B] = Par.map(a)(f)
    def run = Par.run(a)
  }

  object Par {
    private val defaultES = Executors.newCachedThreadPool()

    def unit[A](a: A): Par[A] = _.submit(() => a)

    def fork[A](a: => Par[A]): Par[A] = a(_)

    def async[A](a: => A): Par[A] = fork(unit(a))

    def asyncF[A,B](f: A => B): A => Par[B] = a => async(f(a))

    def run[A](a: Par[A])(s: ExecutorService = defaultES, awaitTimeMs: Long = 1000L): A =
      a(s).get(awaitTimeMs, TimeUnit.MILLISECONDS)

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      unit(f(a.run, b.run))

    def map[A,B](fa: Par[A])(f: A => B): Par[B] =
      map2(fa, unit(()))((a,_) => f(a))

    def product[A,B](fa: Par[A], fb: Par[B]): Par[(A,B)] = fa.map2(fb)((a,b) => (a,b))
  }



  def sum(as: IndexedSeq[Int]): Par[Int] =
    if (as.isEmpty) Par.unit(0)
    else {
      val (l,r) = as.splitAt(as.length/2)
      Par.fork(sum(l)).map2(Par.fork(sum(r)))(_ + _)
    }

  def sortPar(l: Par[List[Int]]): Par[List[Int]] = l.map(_.sorted)
}
