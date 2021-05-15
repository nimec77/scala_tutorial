package data_structures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(n) => n
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

  def map[A, B](t: Tree[A], f: A => B): Tree[B] =
    t match {
      case Leaf(value) => Leaf(f(value))
      case Branch(l, r) => Branch(map(l, f), map(r, f))
    }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B =
    t match {
      case Leaf(value) => f(value)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthVieFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((a, b) => 1 + (a max b))

  def mapViaFold[A, B](t: Tree[A], f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  def main(args: Array[String]): Unit = {
    val t = Branch[Int](Leaf(2), Leaf(3))
    println(size(t))
    println(maximum(t))
    println(depth(t))
    println(map[Int, Int](t, _ + 1))
    println(mapViaFold[Int, Int](t, _ * 2))
  }
}
