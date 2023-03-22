package u03.aula

case class Point3d(x: Double, y: Double, z: Double)

enum Option[A]:
  case Some(a: A)
  case None()

object Option:
  def map[A, B](opt: Option[A], f: A => B): Option[B] = opt match
    case Some(a: A) => Some(f(a))
    case None() => None()

enum MyList[A]:
  case Cons(head: A, tail: MyList[A])
  case Nil()

object MyList:
  import Option.*
  def sum(list: MyList[Int]): Int = list match
    case Cons(h, t) => h + sum(t)
    case Nil() => 0

  def get[E](list: MyList[E], index: Int): Option[E] = (list, index) match
    case (Nil(), _) => None()
    case (Cons(h, _), 0) => Some(h)
    case (Cons(_, t), i) => get(t, i-1)
    // case i < 0 managed by iterating the entire list, then return None()

  def add[E](l: MyList[E], i: Int, elem: E): Option[MyList[E]] = (l, i) match
    case (l, 0) => Some(Cons(elem, l))
    case (Nil(), _) => None()
    case (Cons(h, t), i) => map(add(t, i-1, elem), Cons(h,_))

    /* Generalized above
    case (Cons(h, t), i) => add(t, i-1, elem) match
      case Some(l) => Some(Cons(h, l))
      case None() => None()
    */
@main def exec() =
  import Option.*

  var p = Point3d(10.0, -3.4, 5.6)
  println(p == Point3d(10.0, -3.4, 5.6))
  val d = p match { case Point3d(x, _, _) => x }
  // che cos'è la composizione

  val v = Some(2)
