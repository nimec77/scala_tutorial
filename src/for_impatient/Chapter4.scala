package for_impatient

object Chapter4 {

  def minMax(values: Array[Int]): (Int, Int) = {
    if (values.length == 1)
    (values(0), values(0))
    else {
      var min = values(0)
      var max = values(0)
      for (item <- values) {
        if (item < min) {
          min = item
        } else if (item > max) {
          max = item
        }
      }
      (min, max)
    }
  }


  def main(args: Array[String]): Unit = {
    val prices = Map("MaBook" -> 2000, "IPhone 13" -> 1000)

    val discounts = for ((k, v) <- prices) yield (k, v * 0.9)

    println(discounts)

    println(minMax(Array[Int](1, 2, 4, -5, 6, 3, 7)))
  }
}
