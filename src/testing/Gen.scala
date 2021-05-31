package testing

import state._
import testing.Prop._
import laziness._
import parallelism.Par
import parallelism.Par.{Par, unit}
import testing.Gen.{choose, weighted}

import java.util.concurrent.{ExecutorService, Executors}
import scala.annotation.tailrec
import scala.language.implicitConversions

case class Prop(run: (MaxSize, TestCases, Rng) => Result) {
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Passed | Proved => p.run(max, n, rng)
        case x => x
      }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
        case x => x
      }
  }

  def tag(msg: String): Prop = Prop {
    (max, n, rng) =>
      run(max, n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
  }
}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  case object Proved extends Result {
    override def isFalsified: Boolean = false
  }

  def randomStream[A](g: Gen[A])(rng: Rng): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case $s\n" +
      s"generated an exception: ${e.getMessage} \n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def apply(f: (TestCases, Rng) => Result): Prop =
    Prop { (_, n, rng) => f(n, rng) }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def run(p: Prop, maxSize: MaxSize = 100, testCases: TestCases = 100,
          rng: Rng = Rng.SimpleRng(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
    }

  val Es: ExecutorService = Executors.newCachedThreadPool
  val p1: Prop = Prop.forAll(Gen.unit(Par.unit(1)))(i => Par.map(i)(_ + 1)(Es).get == Par.unit(2)(Es).get)

  def check(p: => Boolean): Prop = Prop { (_, _, _) => if (p) Passed else Falsified("()", 0) }

  val p2: Prop = check {
    val p = Par.map(Par.unit(1))(_ + 1)
    val p2 = Par.unit(2)
    p(Es).get == p2(Es).get
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  val p3: Prop = check {
    equal(
      Par.map(Par.unit(1))(_ + 1),

      Par.unit(2)
    )(Es).get
  }

//  val s = weighted(
//    choose(1,4).map(Executors.newFixedThreadPool) -> .75,
//    unit(Executors.newCachedThreadPool) -> .25) // `a -> b

  //  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
  //    forAll()

}

sealed trait Status {}

object Status {
  case object Exhausted extends Status

  case object Proven extends Status

  case object Unfalsified extends Status
}

case class Gen[+A](sample: State[Rng, A]) {

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => this.listOfN(n))

  def listOf: SGen[List[A]] = Gen.listOf(this)

  def listOf1: SGen[List[A]] = Gen.listOf1(this)

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g: Gen[B]): Gen[(A, B)] =
    (this map2 g) ((_, _))

}

object Gen {

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  val boolean: Gen[Boolean] =
    Gen(State(Rng.boolean))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(Rng.nonNegativeInt).map(n => start + n % (stopExclusive - start)))


  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))

  implicit def uniszed[A](g: Gen[A]): SGen[A] = SGen(_ => g)

  def even(start: Int, stopExclusive: Int): Gen[Int] = {
    choose(start, if (stopExclusive % 2 == 0) stopExclusive - 1 else stopExclusive).
      map(n => if (n % 2 != 0) n + 1 else n)
  }

  def odd(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive % 2 != 0) stopExclusive - 1 else stopExclusive).
      map(n => if (n % 2 == 0) n + 1 else n)

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] = for {
    i <- choose(from, to)
    j <- if (i % 2 == 0) even(from, to) else odd(from, to)
  } yield (i, j)

  def listOfN_1[A](n: Int, g: Gen[A]): Gen[List[A]] =
    List.fill(n)(g).foldRight(unit(List[A]()))((a, b) => a.map2(b)(_ :: _))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(State(Rng.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0, 127)).map(_.map(_.toChar).mkString)

  val string: SGen[String] = SGen(stringN)

  implicit def unsized[A](g: Gen[A]): SGen[A] = SGen(_ => g)

  val smallInt: Gen[MaxSize] = Gen.choose(-10, 10)

  val maxProp: Prop = forAll(listOf(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max)
  }

  val maxProp1: Prop = forAll(listOf1(smallInt)) { l =>
    val max = l.max
    !l.exists(_ > max)
  }

  val sortedProp: Prop = forAll(listOf(smallInt)) { l =>
    val ls = l.sorted
    l.isEmpty || ls.tail.isEmpty || !ls.zip(ls.tail).exists { case (a, b) => a > b }
  }

  object ** {
    def unapply[A, B](p: (A, B)): Option[Nothing] = Some(p)
  }

  lazy val pint2: Gen[Par[Int]] = choose(-100, 100).listOfN(choose(0, 20)).map(l =>
    l.foldLeft(Par.unit(0))((p, i) =>
      Par.fork {
        Par.map2(p, Par.unit(i))(_ + _)
      }
    )
  )

  def getStringIntFn(g: Gen[Int]): Gen[String => Int] =
    g map (i => _ => i)
}

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] =
    SGen {
      g(_) map f
    }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val g2: Int => Gen[B] = n => {
      g(n) flatMap {
        f(_).g(n)
      }
    }
    SGen(g2)
  }

  def **[B](s2: SGen[B]): SGen[(A, B)] =
    SGen(n => apply(n) ** s2(n))

}