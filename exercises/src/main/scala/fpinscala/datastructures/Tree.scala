package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object TreeTest {
  import Tree._
  def main(args: Array[String]) {
    val tree = Branch(Leaf(1), Branch(Leaf(10), Branch(Leaf(3), Leaf(7))))
    println("size", size(tree))
    println("max", maximum(tree))
    println("depth", depth(tree))
    println("map"+ map(tree)(_ + 1))
  }



}
object Tree {

  def fold[A, B](tree: Tree[A])(lf: A => B)(bf: (B, B) => B): B = tree match {
    case Leaf(a) => lf(a)
    case Branch(l, r) => bf(fold(l)(lf)(bf), fold(r)(lf)(bf))
  }

  def size[A](tree: Tree[A]) = fold(tree)(_ => 1)(_ + _)

  def maximum(tree: Tree[Int]) = fold(tree)(identity)(math.max)

  def depth[A](tree: Tree[A]) = fold(tree)(_ => 0)(math.max(_, _) + 1)

  def map[A, B](tree: Tree[A])(f: A => B) = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}