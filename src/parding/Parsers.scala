package parding

import testing.Prop._
import testing._

import scala.language.implicitConversions
import scala.util.matching.Regex

trait Parsers[Parser[+_]] {
  self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(f andThen succeed)

  def defaultSucceed[A](a: A): Parser[A] = string("") map (_ => a)

  def succeed[A](a: A): Parser[A]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {a <- p; b <- p2} yield f(a, b)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p)(a => map(p2)(b => (a, b)))

  def lablel[A](msg: String)(p: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {

    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def many: Parser[List[A]] = self.many(p)

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      self.flatMap(p)(f)

  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))
  }
}


case class ParseError(stack: List[(Location, String)] = List()) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)
}

case class Location(input: String, offset: Int = 0) {

}

object Parsers {

}
