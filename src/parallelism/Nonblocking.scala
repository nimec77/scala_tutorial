package parallelism

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

object Nonblocking {
  trait Future[+A] {
    private[parallelism] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {
    def run[A](es: ExecutorService)(p: Par[A]): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es) { a => ref.set(a); latch.countDown() }
      latch.await()
      ref.get
    }

    def unit[A](a: A): Par[A] =
      _ => (cb: A => Unit) => cb(a)

    def delay[A](a: => A): Par[A] =
      _ => (cb: A => Unit) => cb(a)


    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] {
        override def call(): Unit = r
      })

    def fork[A](a: => Par[A]): Par[A] =
      es => (cb: A => Unit) => eval(es)(a(es)(cb))

    def async[A](f: (A => Unit) => Unit): Par[A] =
      _ => (cb: A => Unit) => f(cb)


    def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
      es => (cb: C => Unit) => {
        var ar: Option[A] = None
        var br: Option[B] = None

        val combiner = Actor[Either[A, B]](es) {
          case Left(a) =>
            if (br.isDefined) eval(es)(cb(f(a, br.get)))
            else ar = Some(a)

          case Right(b) =>
            if (ar.isDefined) eval(es)(cb(f(ar.get, b)))
            else br = Some(b)
        }
        p1(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }

    def map[A, B](p: Par[A])(f: A => B): Par[B] =
      es => (cb: B => Unit) => p(es)(a => eval(es) {
        cb(f(a))
      })

    def lazyUnit[A](a: => A): Par[A] =
      fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] = as match {
      case Nil => unit(Nil)
      case h :: t => map2(h, fork(sequence(t)))(_ :: _)
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
      sequence(as.map(asyncF(f)))

    def choice[A](p: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es => (cb: A => Unit) => p(es) { b =>
        if (b) eval(es) {
          t(es)(cb)
        }
        else eval(es) {
          f(es)(cb)
        }
      }

    def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] =
      es => (cb: A => Unit) => p(es) { ind =>
        eval(es) {
          ps(ind)(es)(cb)
        }
      }

    def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
      choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))

    def choiceMap[K, V](p: Par[K])(ps: Map[K, Par[V]]): Par[V] =
      es => (cb: V => Unit) => p(es)(k => ps(k)(es)(cb))

    def flatMap[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
      es => (cb: B => Unit) => p(es)(a => f(a)(es)(cb))

    def chooser[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
      flatMap(p)(f)

    def choiceViaFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
      flatMap(p)(b => if (b) t else f)

    def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
      flatMap(p)(i => choices(i))

    def join[A](p: Par[Par[A]]): Par[A] =
      es => (cb: A => Unit) => p(es)(p2 => eval(es) {
        p2(es)(cb)
      })

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
      flatMap(a)(x => x)

    def flatMapViaJoin[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
      join(map(p)(f))

    class ParOps[A](p: Par[A]) {
      def map[B](f: A => B): Par[B] = Par.map(p)(f)
      def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, b)(f)
      def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)
//      def zip[B](b: Par[B]): Par[(A, B)] = p.map2(b)((_, _))
    }
  }
}
