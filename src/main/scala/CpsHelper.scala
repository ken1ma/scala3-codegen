import scala.util.Try

import cats.*
import cats.syntax.all.*
import cps.CpsTryMonad

object CpsHelper:
   /*
      async and await support
      https://typelevel.org/cats-effect/docs/std/async-await
      https://rssh.github.io/dotty-cps-async/

      cats-effect-cps 0.3.0 provides only for Async
      and doobie doesn't seem to provide Async
      https://github.com/typelevel/cats-effect-cps/blob/v0.3.0/core/shared/src/main/scala-3/cats/effect/cps.scala

      cps-async-connect could be used
      https://github.com/rssh/cps-async-connect/blob/master/cats-effect/shared/src/main/scala/cps/monads/catsEffect/CatsAsync.scala

      https://github.com/rssh/dotty-cps-async/blob/master/shared/src/main/scala/cps/CpsMonad.scala
      CpsTryMonad

      doobie propagates exception rather than using Either
      https://tpolecat.github.io/doobie/docs/09-Error-Handling.html

      doobie instance
      https://github.com/tpolecat/doobie/blob/v1.0.0-RC1/modules/free/src/main/scala/doobie/WeakAsync.scala
   */
   given [F[_]](using F: MonadError[F, Throwable]): CpsTryMonad[F] with
      def pure[A](a: A): F[A] = F.pure(a)
      def map[A, B](fa: F[A])(f: A => B): F[B] = F.map(fa)(f)
      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = F.flatMap(fa)(f)
      def flatMapTry[A, B](fa: F[A])(f: Try[A] => F[B]): F[B] = F.flatMap(F.attempt(fa))(ex => f(ex.toTry))
      def error[A](ex: Throwable): F[A] = F.raiseError(ex)
