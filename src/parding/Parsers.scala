package parding

import scala.language.implicitConversions

trait Parsers[Parser[+_]] {
  self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A](s1: Parser[A], s: Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    flatMap(a)(f andThen succeed)

  def defaultSucceed[A](a: A): Parser[A] = string("") map (_ => a)

  def succeed[A](a: A): Parser[A]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]


  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
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
