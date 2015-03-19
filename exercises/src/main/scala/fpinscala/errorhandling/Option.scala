package fpinscala.errorhandling


import scala.annotation.tailrec
import scala.{Either => _, Option => _, Some => _}

// hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(n => Math.pow(n - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(av => b.map(bv => f(av, bv)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a.foldLeft(Some(Nil): Option[List[A]])((acc, el) => map2(acc, el)((a, e) => e :: a))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    def loop(a: List[A], acc: Option[List[B]]): Option[List[B]] = acc.flatMap(accL => a match {
      case Nil => acc
      case x :: xs => loop(xs, f(x).map(_ :: accL))
    })

    loop(a, Some(Nil))
  }

  def traverse_tailRec[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    @tailrec
    def loop(a: List[A], acc: Option[List[B]]): Option[List[B]] = acc match {
      case None => None
      case Some(list) => a match {
        case Nil => acc
        case x :: xs => loop(xs, f(x).map(_ :: list))
      }
    }

    loop(a, Some(Nil))
  }

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case x :: xs => map2(f(x), traverse_1(xs)(f))((v, l) => v :: l)
  }

  def sequenceUsingTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(e => e)
}