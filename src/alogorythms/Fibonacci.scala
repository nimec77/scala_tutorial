package alogorythms

object Fibonacci {

  def fibClassic(n: Int): Long = {
    var prev = 0L
    var next = 1L
    for (_ <- 0 until n) {
      val temp = next
      next = prev + next
      prev = temp
    }

    prev
  }

  def mul(ar: Array[Array[Long]], br: Array[Array[Long]]): Array[Array[Long]] = {
    val a1 = ar(0)(0)
    val a2 = ar(0)(1)
    val a3 = ar(1)(0)
    val a4 = ar(1)(1)

    val b1 = br(0)(0)
    val b2 = br(0)(1)
    val b3 = br(1)(0)
    val b4 = br(1)(1)

    Array(
      Array(a1 * b1 + a2 * b3, a1 * b2 + a2 * b4),
      Array(a3 * b1 + a4 * b3, a3 * b2 + a4 * b4),
    )
  }

  def fib(n: Int): Long = {
    val matrix = Array(
      Array(0L, 1L),
      Array(1L, 1L),
    )

    var result = Array(
      Array(1L, 0L),
      Array(0L, 1L),
    )

    val bits = n.toBinaryString
    for (bit <- bits) {
      result = mul(result, result)
      if (bit == '1') {
        result = mul(result, matrix)
      }
    }

    result(1)(0)
  }

  def mulPow(x: Long, n: Long): BigInt = n match {
    case 1 => BigInt(x)
    case n if n % 2 == 0 => val y = mulPow(x, n / 2); y * y
    case _ => BigInt(x) * mulPow(x, n - 1)
  }

  def main(args: Array[String]): Unit = {
    println(fib(76))
    println(fibClassic(76))
    println(mulPow(15319, 91040))
  }
}
