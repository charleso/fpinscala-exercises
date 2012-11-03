package fpinscala.laziness

import Stream._

object StreamTest {

  import Stream._
  def main(args: Array[String]) {
    println("toList", Stream("a", "b", "c").toList)
    println("take", Stream("a", "b", "c", "d", "e").take(2).toList)
    println("take", ones.take(4).toList)
    val long = cons(1, cons(2, cons(3, cons(4, ones))))
    println("takeWhile", Stream(1,2,3,4,5,6,7).takeWhile(_ < 4).toList)
    println("forAll", long.forAll(_ < 4))
    println("forAll", Stream(1,2,3,4,5,6,7).forAll(_ < 14))
    println("fusion", long.map(_ + 1).filter(_ < 10).take(10).toList)
    println("from", from(5).take(10).toList)
    println("startsWith", startsWith(long, Stream(1,2,3)))
    println("startsWith", startsWith(long, Stream(1,2,4)))
    println("tails", Stream(1,2,3).tails.map(_.toList).toList)
    println("append", Stream(1,2,3).append(Stream(4, 5, 6)).toList)
  }
}

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]
  def isEmpty: Boolean = uncons.isEmpty
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

  def toList: List[A] = foldRight(List[A]())(::(_, _))

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b)

  def take(n: Int): Stream[A] =
    if (n == 0) empty
    else uncons.map {
      case (a, s) => cons(a, s.take(n - 1))
    }.getOrElse(empty)

  def takeWhileRec(p: A => Boolean): Stream[A] = uncons match {
    case Some((a, s)) => if (p(a)) cons(a, s.takeWhile(p)) else empty
    case None => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, t) => if (p(a)) cons(a, t) else empty )

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, t) => cons(f(a), t) )

  def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] = zip(b).map(f.tupled)

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, t) => f(a).append(t) )

  def append[B >: A](s: Stream[B]) = foldRight(s)(cons(_, _))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, t) => if (p(a)) cons(a, t) else t )

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def zip[B](s2: Stream[B]): Stream[(A, B)] = unfold(this, s2) {
    case (s1, s2) => s1.uncons.flatMap(a => s2.uncons.map(b => ((a._1, b._1), (a._2, b._2))))
  }

  def tailsWTF: Stream[Stream[A]] = unfold(Stream(this))(_.uncons.flatMap(y => y._1.uncons.map(x => (y._1, Stream(x._2)))))
  def tails: Stream[Stream[A]] = unfold(this)(_.uncons.map(x => (cons(x._1, x._2), x._2)))

}
object Stream {

  def empty[A]: Stream[A] = 
    new Stream[A] { def uncons = None }
  
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = 
    new Stream[A] {
      lazy val uncons = Some((hd, tl)) 
    }
  
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constantEasy[A](a: A): Stream[A] = cons(a, constant(a))
  def constant[A](a: A): Stream[A] = unfold(a)(Some(a, _))
  val ones: Stream[Int] = constant(1)
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  def startsWith[A](s: Stream[A], s2: Stream[A]): Boolean = s.zip(s2).forAll(x => x._1 == x._2)

}