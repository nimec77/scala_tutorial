package for_impatient

import scala.annotation.tailrec

object Chapter2 {

  def signum(a: Int): Int = {
    if (a < 0) -1
    else if (a == 0) 0
    else 1
  }

  def countdown(n: Int): Unit = for (i <- n to 0 by -1) print(s"$i ")

  def pow(x: Int, n: Int): Double = {
    if (n > 0)
      if (n % 2 == 0) {
        val y = pow(x, n / 2)
        y * y
      } else x * pow(x, n - 1)
    else if (n < 0) 1 / pow(x, -n)
    else 1
  }

  def main(args: Array[String]): Unit = {
    //    println(s"signum(-19)=${signum(-19)}")
    //    println(s"signum(0)=${signum(0)}")
    //    println(s"signum(10)=${signum(10)}")
    //    for (i <- 10 to 0 by -1) print(s"$i ")
    //    println(countdown(10))
    //    var product = 1
    //    for (c <- "Hello") product *= c
    //    println(product)
    //    val product = "Hello".map(a => a.toInt).product
    //    println(product)
    print(s"${pow(5, -3)}")
  }
}
