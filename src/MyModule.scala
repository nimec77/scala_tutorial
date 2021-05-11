import scala.annotation.tailrec

object MyModule {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  def iSorted[A] (as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(n: Int): Boolean = {
      if (n>= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else go(n + 1)
    }

    go(0)
  }

  def partail1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  private def formatAbs(x: Int): String = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int): String = {
    val msg = "The factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  private def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
  }

}
