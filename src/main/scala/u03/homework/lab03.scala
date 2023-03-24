package u03.homework

import scala.annotation.tailrec


object Part1:

  import u03.Lists.List
  import u03.Lists.List.*
  import u02.Optionals.Option.*
  import u02.Optionals.Option

  def drop[A](l: List[A], n: Int): List[A] = (l, n) match
    case (Cons(_, t), 1) => t
    case (Cons(_, Nil()), _) => Nil()
    case (Cons(h, t), _) => Cons(h, drop(t, n-1))
    case (Nil(), _) => Nil()

  def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
    case (Cons(h, Nil()), r) => Cons(h, r)
    case (Cons(h, t), r) => Cons(h, append(t, r))
    case (Nil(), r) => r

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
    case Nil() => Nil()
    case Cons(h, Nil()) => f(h)
    case Cons(h, t) => f(h) match
      case r => append(r, flatMap(t)(f))

  def mapList[A, B](l: List[A])(mapper: A => B): List[B] =
    flatMap(l)(el => Cons(mapper(el), Nil()))

  def filterList[A](l: List[A])(predicate: A => Boolean): List[A] =
    flatMap(l)(el => predicate(el) match
      case true => Cons(el, Nil())
      case false => Nil()
    )

  private def value(v: Option[Int]): Int = v match
    case None() => Int.MinValue
    case Some(v) => v

  def max(l: List[Int]): Option[Int] = l match
    case Nil() => None[Int]()
    case Cons(h, t) => value(max(t)) match
      case v if h > v => Some(h)
      case v => Some(v)

object Part1MoreOnLists:

  import Part1.flatMap
  import u03.Lists.List
  import u03.Lists.List.*
  import u02.Modules.Person.*
  import u02.Modules.Person


  def getCourses(l: List[Person]): List[String] =
    flatMap(l) { case Teacher(_, course) => Cons(course, Nil()); case _ => Nil() }

  @tailrec
  def foldLeft[A, B](l: List[A])(acc: B)(op: (B, A) => B): B = l match
    case Cons(h, Nil()) => op(acc, h)
    case Cons(h, t) => foldLeft(t)(op(acc, h))(op)

  def foldRight[A, B](l: List[A])(acc: B)(op: (A, B) => B): B = l match
    case Cons(h, Nil()) => foldLeft(Cons(h, Nil()))(acc)((b, a) => op(a, b))
    case Cons(h, t) => foldRight(Cons(h, Nil()))(foldRight(t)(acc)(op))(op)

