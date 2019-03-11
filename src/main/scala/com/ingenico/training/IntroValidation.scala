package com.ingenico.training

import scala.util.{Failure, Success, Try}

object IntroValidation {

  def map[A, B](input: Option[A])(f: A => B): Option[B] = {
    input match {
      case Some(v) => Some(f(v))
      case None => None
    }
  }

  case class X3(x1: Int, x2: Long, x3: String)

  def map3[A1, A2, A3, A](input: (Option[A1], Option[A2], Option[A3]))(f: (A1, A2, A3) => A): Option[A] = {
    input match {
      case (Some(x1), Some(x2), Some(x3)) => Some(f(x1, x2, x3))
      case _ => None
    }
  }

  case class Person(name: String, age: Int)


  val data = Map("name" -> "David", "age" -> "xxx")

  def validate(input: Map[String, String]): Try[Person] = {
    for {
      name <- Try(input("name"))
      age <- Try(input("age").toInt)
    } yield Person(name, age)
  }


  sealed trait Validated[+E, +A]
  case class NonEmptyList[A](first: A, rest: List[A])

  case class Invalid[E](errors: NonEmptyList[E]) extends Validated[E, Nothing]
  case class Valid[A](value: A) extends Validated[Nothing, A]

  sealed trait ValidationError
  case class FieldNotExistError(fieldName: String) extends ValidationError
  case class NumberFormatError(fieldName: String) extends ValidationError

  def validateString(input: Map[String, String], fieldName: String): Validated[ValidationError, String] = {
    input.get(fieldName) match {
      case Some(v) => Valid(v)
      case None => Invalid(NonEmptyList(FieldNotExistError(fieldName), Nil))
    }
  }

  def validateInt(input: Map[String, String], fieldName: String): Validated[ValidationError, Int] = {
    input.get(fieldName) match {
      case Some(v) => Try(v.toInt) match {
        case Success(int) => Valid(int)
        case Failure(exception) => Invalid(NonEmptyList(NumberFormatError(fieldName), Nil))
      }
      case None => Invalid(NonEmptyList(FieldNotExistError(fieldName), Nil))
    }
  }

  def validatePerson(input: Map[String, String]): Validated[ValidationError, Person] = {
    val validatedName = validateString(input, "name")
    val validatedAge = validateInt(input, "age")

    (validatedName, validatedAge) match {
      case (Valid(name), Valid(age)) => Valid(Person(name, age))
      case (Invalid(nameError), Valid(_)) => Invalid(nameError)
      case (Valid(_), Invalid(ageError)) => Invalid(ageError)
      case (Invalid(errorName), Invalid(errorAge)) => Invalid(
        NonEmptyList(errorName.first, errorAge.first :: errorName.rest)
      )
    }
  }



  def main(args: Array[String]): Unit = {
    val emptyMap = Map[String, String]()
    val result = validate(emptyMap)

    val result2 = validatePerson(emptyMap)
    println(result)

    println(result2)
  }

}
