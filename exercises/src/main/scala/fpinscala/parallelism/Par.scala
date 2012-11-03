package fpinscala.parallelism

import java.util.concurrent._

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = _ => new Future[A] {
    def cancel(b: Boolean) = false
    def isCancelled() = false
    def isDone() = true
    def get() = a
    def get(l: Long, timeUnit: TimeUnit) = a
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val af = a(es)
    val bf = b(es)
    new Future[C] {
      def cancel(b: Boolean) = af.cancel(b) || bf.cancel(b)
      def isCancelled() = af.isCancelled || bf.isCancelled
      def isDone() = af.isDone && bf.isDone
      def get() = f(af.get, bf.get)
      def get(l: Long, timeUnit: TimeUnit) = f(af.get(l, timeUnit), bf.get(l, timeUnit))
    }
  }

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call(): A = run(es)(a).get
  })

  def asyncF[A,B](f: A => B): A => Par[B] = a => unit(f(a))

  def map[A,B](fa: Par[A])(f: A => B): Par[B] = map2(fa, unit(()))((a,_) => f(a))

}

object Examples {
  def sum(as: IndexedSeq[Int]): Int =
    if (as.size <= 1) as.headOption getOrElse 0 // Hints and standalone answers
    else {
      val (l,r) = as.splitAt(as.length/2)
      sum(l) + sum(r)
    }

}