package applicative

import monads.Functor
import monoids.Monoid
import state.State

trait Applicative[F[_]] extends Functor[F] {
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_ (_))

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def factor[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({
      type f[x] = (F[x], G[x])
    })#f] {
      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

      override def apply[A, B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    }
  }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    (ofa foldLeft unit(Map.empty[K, V])) { case (acc, (k, fv)) =>
      map2(acc, fv)((m, v) => m + (k -> v))
    }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E]) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Applicative {
  val streamApplicative: Applicative[Stream] = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] =
      Stream.continually(a)

    override def map2[A, B, C](a: Stream[A], b: Stream[B])(f: (A, B) => C): Stream[C] = a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
    new Applicative[({
      type f[x] = Validation[E, x]
    })#f] {
      override def unit[A](a: => A): Validation[E, A] = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
        (fa, fb) match {
          case (Success(a), Success(b)) => Success(f(a, b))
          case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
          case (e@Failure(_, _), _) => e
          case (_, e@Failure(_, _)) => e
        }
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]): Applicative[({
    type f[x] = Const[M, x]
  })#f] =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      override def unit[A](a: => A): Const[M, A] = M.zero

      override def apply[A, B](m1: Const[M, A => B])(m2: Const[M, A]): Const[M, B] = M.op(m1, m2)
    }
}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  override def apply[A, B](mf: F[A => B])(ma: F[A]): F[B] = flatMap(mf)(f => map(ma)(f))

  override def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)
}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({
      type f[x] = Either[E, x]
    })#f] {
      override def unit[A](a: => A): Either[E, A] = Right(a)

      override def flatMap[A, B](eea: Either[E, A])(f: A => Either[E, B]): Either[E, B] = eea match {
        case Right(a) => f(a)
        case Left(b) => Left(b)
      }
    }

  def stateMonad[S]: Monad[({
    type f[x] = State[S, x]
  })#f] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] = st flatMap f
  }

//  def composeM[G[_], H[_]](implicit G: Monad[G], H: Monad[H], T: Traverse[H]):
//  Monad[({type f[x] = G[H[x]]})#f] = new Monad[({
//  type f[x] = G[H[x]]
//})#f] {
//    override def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))
//
//    override def flatMap[A, B](mna: G[H[A]])(f: A => G[H[B]]): G[H[B]] = G.flatMap(mna)(na => G.map(T.traverse(na)(f))(H.join))
//  }
}

//trait Traverse[F[_]] extends Functor[F] with Foldable[F] {self =>
//  def traverse[M[_]: Applicative, A, B](fa: F[A])(f: A=> M[B]): M[F[B]] =
//    sequence(map(fa)(f))
//}
