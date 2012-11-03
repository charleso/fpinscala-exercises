package fpinscala.datastructures

import annotation.tailrec

sealed trait List[+A] // `List` data type
case object Nil extends List[Nothing] // data constructor for `List`
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object ListMain {

  import List._

  def main(args: Array[String]) {
    println("tail", tail(List("a", "b", "c", "d")))
    println("drop", drop(List("a", "b", "c", "d"), 2))
    println("dropWhile", dropWhile(List("a", "b", "c", "d"))(_ != "c"))
    println("setHead", setHead(List("a", "b", "c", "d"))("x"))
    println("reverse", reverse(List("a", "b", "c", "d")))
    println("length", length(List("a", "b", "c", "d")))
    println("init", init(List("a", "b", "c", "d")))
    println("map", map(List("a", "b", "c", "d"))(_.toUpperCase))
    println("flatten", flatten(List(List("a", "b"), List("c", "d"))))
    println("foldLeft", foldLeft(List("a", "b", "c", "d"), "e")((b, a) => a + b))
    println("foldLeftHard", foldLeftViaFoldRight(List("a", "b", "c", "d"), "e")((b, a) => a + b))
  }

}

object List { // `List` companion object

  def sum(ints: List[Int]): Int = ints match { // Pattern matching example
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  
  val example = Cons(1, Cons(2, Cons(3, Nil))) // Creating lists
  val example2 = List(1,2,3)
  val total = sum(example)

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(l: List[Int]) = 
    foldRight(l, 0.0)(_ + _)
  
  def product2(l: List[Double]) = 
    foldRight(l, 1.0)(_ * _)


  def last[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("No more list")
    case t@Cons(_, Nil) => t
    case Cons(h, t) => tail(t)
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = if (n == 0) l else drop(tail(l), n - 1)

  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case l@Cons(h, t) => if (f(h)) dropWhile(t)(f) else l
  }

  def setHead[A](l: List[A])(h: A): List[A] = Cons(h, tail(l))

  def init[A](l: List[A]): List[A] = reverse(tail(reverse(l)))

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]()) {
    case (l, h) => Cons(h, l)
  }

  def length[A](l: List[A]): Int = foldLeft(l, 0)((l, _) => l + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(l), z) ((a, b) => f(b, a))

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]()) {
    case (a, b) => Cons(f(a), b)
  }

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  def flatten[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())(append2)
}