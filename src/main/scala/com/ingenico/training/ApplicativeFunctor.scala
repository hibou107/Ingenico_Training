package com.ingenico.training

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait ApplicativeFunctor[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]

  def map2[A, B, C](first: F[A], second: F[B])(f: (A, B) => C): F[C]

  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, pure(f)) {
    case (faValue, fValue) => fValue(faValue)
  }

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = map2(fa, ff){ (value, function) =>
    function(value)
  }
}


object optionApplicativeFunctor extends ApplicativeFunctor[Option] {
  override def pure[A](a: A): Option[A] = Some(a)

  override def map2[A, B, C](first: Option[A], second: Option[B])(f: (A, B) => C): Option[C] =
    (first, second) match {
      case (Some(firstValue), Some(secondValue)) => pure(f(firstValue, secondValue))
      case _ => None
    }
}

object ApplicativeFunctor {

  implicit def eitherApplicative[L]: ApplicativeFunctor[({type T[A] = Either[L, A] })#T] = new ApplicativeFunctor[({
  type T[A] = scala.Either[L, A]})#T] {
    override def pure[A](a: A): Either[L, A] = Right(a)

    override def map2[A, B, C](first: Either[L, A], second: Either[L, B])(f: (A, B) => C): Either[L, C] =
      (first, second) match {
        case (Right(firstValue), Right(secondValue)) => Right(f(firstValue, secondValue))
        case (Left(firstValue), _) => Left(firstValue)
        case (_, Left(secondValue)) => Left(secondValue)
      }
  }
}

