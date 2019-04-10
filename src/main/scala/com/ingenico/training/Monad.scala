package com.ingenico.training


trait Monad[F[_]] extends ApplicativeFunctor[F]{
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def flatten[A](ffa: F[F[A]]): F[A] = {
    flatMap(ffa)(identity)
  }

  override def map[A, B](fa: F[A])(f: A => B): F[B] = {
    flatMap(fa)(x => pure(f(x)))
  }

  override def map2[A, B, C](first: F[A], second: F[B])(f: (A, B) => C): F[C] = flatMap(first)(x => map(second)(y => f(x, y)))


}

object MonadOption extends Monad[Option] {
  override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = ???

  override def pure[A](a: A): Option[A] = ???
}

object Main {
  type Id[A] = A
  object IdentityMonad extends Monad[Id] {
    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)

    override def pure[A](a: A): Id[A] = a
  }


  def myMain(): Option[Int] = {
    val x = f(2)
    x.map(_ + 2)
  }

  def f(a: Int): Option[Int] = Some(a)

}


