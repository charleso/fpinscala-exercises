package fpinscala.structuringprograms

import annotation.tailrec

case class Box(height: Double, width: Double)

object Exercises {

  def main(args: Array[String]) {
    println("abs", absolute((_:Int) + 1)(-3))
    println("even", 1 to 10 map even)
    println("fizzbuzz", 1 to 10 map fizzbuzz)
    println("fib", 1 to 10 map fib)
    println("fib", 1 to 10 map fib2)
    println("sqrt", sqrt(9))
  }

  def greaterBy(x: Box, y: Box, f: Box => Double): Box = 
  if (f(x) > f(y)) x else y

  type Pred[A] = A => Boolean

  def absolute[A](f: A => Int): A => Int = n => math.abs(f(n))

  def even(n: Int): Boolean = divisibleBy(2)(n)

  def divisibleBy(k: Int): Pred[Int] = n => n % k == 0

  def fizzbuzz(n: Int) = liftPreds(_ || _, divisibleBy(3), divisibleBy(5))(n)

  def lift[A,B,C,D](f: (B, C) => D)(g: A => B, h: A => C): A => D = a => f(g(a), h(a))

  def liftPreds[A](f: (Boolean, Boolean) => Boolean,
              g: Pred[A],
              h: Pred[A]): Pred[A] = n => f(g(n), h(n))

  def curry[A,B,C](f: (A, B) => C): A => B => C = a => b => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

  def lift3[A,B,C,D,E](f: (B, C, D) => E)(g: A => B,
                                          h: A => C,
                                          i: A => D): A => E = a => lift((b: B, c: C) => f(b, c, i(a)))(g, h)(a)

  def fib(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case n => fib(n - 2) + fib(n - 1)
  }

  def fib2(n: Int): Int = {
    @tailrec
    def fibRec(n: Int, b: Int, c: Int): Int = if (n == 0) b else fibRec(n - 1, c, b + c)
    fibRec(n, 0, 1)
  }

  def sqrt(n: Double): Double = {
    def f(x: Double) = (x * x) - n // We want to find the `x` such that `x` squared minus `n` equals `0`.
    iterateWhile(2.0)(x => x - f(x) / (2 * x), // Starting with a guess of `2.0`, iteratively improve the guess.
                      x => f(x).abs > 1e-14) // `1e-14` is a way of writing `10` to the `-14`th power, a rather small number. When the difference between the guess and the answer is smaller than this, the guess is "good enough".
  }

  @tailrec
  def iterateWhile[A](a: A)(f: A => A, p: Pred[A]): A = if (!p(a)) a else iterateWhile(f(a))(f, p)
}