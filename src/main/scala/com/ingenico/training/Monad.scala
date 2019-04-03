package com.ingenico.training

trait Monad[F[_]] extends ApplicativeFunctor[F]{
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def flatten[A](ffa: F[F[A]]): F[A] = {
    flatMap(ffa)(identity)
  }

  override def map[A, B](fa: F[A])(f: A => B): F[B] = {
    flatMap(fa)(x => pure(f(x)))
  }

  override def map2[A, B, C](first: F[A], second: F[B])(f: (A, B) => C): F[C] = {
    flatMap(first){ a => map(second)(b => f(a, b))}
  }
}
