package u03.aula

import org.junit.Assert.assertEquals
import org.junit.Test

class AulaListTest:

  /**
   * type List[A]
   * constructors
   *   Cons(head: A, tail: List[A])
   *   Nil()
   * operations:
   *   sum: List[Int] -> int
   *   get[E]: List[E] x Int -> Option[E]
   *   add[E]: List[E] x Int x E -> List[E]
   *
   * axioms:
   *   sum(Nil()) = ???
   *   sum(Cons(h,t)) = ???
   *   get(Nil, _) = None()
   *   get(Cons(h,t), 0) => Some(h)
   *   get(Cons(h,t), i) => get(t, i-1)
  */


  import MyList.*
  import Option.*

  val list: MyList[Int] = Cons(10, Cons(20, Nil()))
  val empty: MyList[Int] = Nil()

  @Test
  def testSum(): Any =
    assertEquals(0, sum(empty))
    assertEquals(30, sum(list))

  @Test
  def testGet(): Any =
    assertEquals(Some(10), get(list, 0))
    assertEquals(Some(20), get(list, 1))
    assertEquals(None(), get(list, 2))
    assertEquals(None(), get(list, -3))
    
  @Test
  def testAdd() =
    assertEquals(Cons(9, Cons(10, Cons(20, Nil()))), add(list, 0, 9))
    assertEquals(Some(Cons(10, Cons(11, Cons(20, Nil())))), add(list, 1, 11))
    assertEquals(Some(Cons(10, Cons(11, Cons(21, Nil())))), add(list, 21, 11))

    assertEquals(None(), add(list, 3, 50))
    assertEquals(None(), add(list, -3, 50))