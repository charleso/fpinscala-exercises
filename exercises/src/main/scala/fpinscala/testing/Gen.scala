package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.state.RNG._
import Gen._
import Prop._

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

object GenTest {

  def main(args: Array[String]) {

  }

}

trait Prop { self =>

  def check: Either[FailedCase, SuccessCount]
  def &&(p: Prop): Prop = new Prop {
    override def check = self.check.right.flatMap(count => p.check.right.map(_ + count))
  }
}

object Prop {

  type FailedCase = String
  type SuccessCount = Int
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = sys.error("placeholder")
}

trait Status {

}

object Status {

}

object Gen {

  implicit def rand2state[A](r: RNG.Rand[A]) = State(r)

  type Domain[+A] = Stream[Option[A]]
  def bounded[A](a: Stream[A]): Domain[A] = a map (Some(_))
  def unbounded: Domain[Nothing] = Stream(None)

  type GenState[A] = State[RNG, A]

  def chooseState(start: Int, stopExclusive: Int): GenState[Int] = map(positiveMax(stopExclusive - start))(_ + start)

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a), bounded(Stream(a)))

  def boolean: Gen[Boolean] = Gen(RNG.boolean, bounded(Stream(true, false)))

  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(chooseState(start, stopExclusive), bounded(Stream.from(start).take(stopExclusive - start)))

  // g.exhaustive.map(List.fill(n)(_))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)),
    cartesian(Stream.constant(g.exhaustive)).map(s => sequenceOption(s.take(n).toList)))

  def sequenceOption[A](l: List[Option[A]]): Option[List[A]] = l.foldRight(Some(Nil): Option[List[A]])(map2Option(_, _)(_ :: _))

  /** Between 0 and 1, not including 1. */
  def uniform: Gen[Double] = Gen(RNG.double, unbounded)

  /** Between `i` and `j`, not including `j`. */
  def choose(i: Double, j: Double): Gen[Double] = uniform.map(d => i + d*(j-i))

  /* `cartesian` generates all possible combinations of a `Stream[Stream[A]]`. For instance:
   *
   *    cartesian(Stream(Stream(1,2), Stream(3), Stream(4,5))) ==
   *    Stream(Stream(1,3,4), Stream(1,3,5), Stream(2,3,4), Stream(2,3,5))
  */
  def cartesian[A](s: Stream[Stream[A]]): Stream[Stream[A]] =
    s.foldRight(Stream(Stream[A]()))(_.zipWith(_)(Stream.cons(_,_)))

  /* `map2Option` and `map2Stream`. Notice the duplication! */
  def map2Option[A,B,C](oa: Option[A], ob: Option[B])(f: (A,B) => C): Option[C] =
    for { a <- oa; b <- ob } yield f(a,b)

}

case class Gen[+A](sample: State[RNG,A], exhaustive: Domain[A]) {
  def map[B](f: A => B) = Gen(sample.map(f), exhaustive.map(_.map(f)))

  def map2[B, C](b: Gen[B])(f: (A, B) => C) = Gen(sample.map2(b.sample)(f),
    exhaustive.zipWith(b.exhaustive)(map2Option(_, _)(f)))

  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample),
    exhaustive.flatMap(_.map(a => f(a).exhaustive).getOrElse(unbounded)))
}
