package com.ingenico.training

import scala.language.higherKinds
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait ApplicativeFunctor[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]

  def map2[A, B, C](first: F[A], second: F[B])(f: (A, B) => C): F[C]

  def map3[A, B, C, D](first: F[A], second: F[B], third: F[C])(f: (A, B, C) => D): F[D] = {
    val tuple = map2(first, second) { (firstValue, secondValue) => (firstValue, secondValue)
    }
    map2(tuple, third) { case ((firstValue, secondValue), thirdValue) =>
        f(firstValue, secondValue, thirdValue)
    }
  }

  def map[A, B](a: F[A])(f: A => B): F[B] = {
    map2(a, pure(f)) { (aValue, fValue) =>
      fValue(aValue)
    }
  }

  def traverse[A](l: List[F[A]]): F[List[A]] = {
    l match {
      case Nil => pure(Nil)
      case x :: xs => map2(x, traverse(xs)) { (first, rest) => first :: rest }
    }
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


trait Monad[F[_]] extends ApplicativeFunctor[F]


object ApplicativeFunctor {

  def compose[F[_]: ApplicativeFunctor, G[_]: ApplicativeFunctor, A]: ApplicativeFunctor[({type T[A] = F[G[A]]})#T] = {
    val F = implicitly[ApplicativeFunctor[F]]
    val G = implicitly[ApplicativeFunctor[G]]
    new ApplicativeFunctor[({type T[A] = F[G[A]]})#T] {
      override def pure[A](a: A): F[G[A]] = F.pure(G.pure(a))

      override def map2[A, B, C](first: F[G[A]], second: F[G[B]])(f: (A, B) => C): F[G[C]] =
        F.map2(first, second) { (ga, gb) =>
          G.map2(ga, gb)(f(_, _))
        }
    }

  }

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

