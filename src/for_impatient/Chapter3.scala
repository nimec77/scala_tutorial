package for_impatient

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Chapter3 {

  val random = new Random()

  def randomInt(min: Int, max: Int): Int = random.nextInt(max - min) + min

  def main(args: Array[String]): Unit = {
        val as = new ArrayBuffer[Int]()
        for (i <- 0 until 10) {
          as += i
        }
        println(as)
        val bs = as.clone()
        for (i <- as.indices by 2) {
          val temp = bs(i)
          bs(i) = as(i + 1)
          bs(i + 1) = temp
        }
        println(bs)

        val cs = for (i <- as.indices by 2; j <- i + 1 to i by -1) yield as(j)

        println(cs)

    val ds = new Array[Int](10)
    for (i <- ds.indices) {
      ds(i) = randomInt(-50, 50)
    }
    println(ds.mkString("Array(", ", ", ")"))

    var fs = for (element <- ds if element > 0) yield element

    val gs = for(element <- ds if element <= 0) yield element

    fs ++= gs

    println(fs.mkString("Array(", ", ", ")"))

    val hs = new ArrayBuffer[Int]()
    val js = new ArrayBuffer[Int]()

    for (element <- ds) {
      if (element > 0) hs += element
      else js += element
    }
    hs ++= js

    println(hs)

    println(as.sum.toDouble / as.length)

    println(ds.sortWith(_ < _).mkString("Array(", ", ", ")"))

    println(ds.map(math.abs).distinct.mkString("Array(", ", ", ")"))

  }
}
