package u03.homework

import org.junit.Test
import org.junit.Assert.{assertEquals, assertNull}
import org.junit.Before

import u03.homework.Part1.*
import u03.Lists.List.*
import u03.Lists.List
case object Part1ListTests:
  import u02.Optionals.Option.Some
  import u02.Optionals.Option.None
  import u02.Optionals.Option

  val list: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))
  class BehaviorTests:
    @Test
    def dropFirst() =
      val expected = Cons(20, Cons(30, Nil()))
      assertEquals(expected, drop(list, 1))

    @Test
    def dropRecursive() =
      val expected = Cons(10, Cons(30, Nil()))
      assertEquals(expected, drop(list, 2))

    @Test
    def appendOneElementLists() =
      val expected = Cons(10, Cons(20, Nil()))
      assertEquals(expected, append(Cons(10, Nil()), Cons(20, Nil())))

    @Test
    def appendArbitraryLengthLists() =
      val toAppend = Cons(40, Cons(50, Nil()))
      val expected = Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil())))))
      assertEquals(expected, append(list, toAppend))

    @Test
    def flatMapIncrementByOneSimpleList() =
      val expected = Cons(11, Nil())
      assertEquals(expected, flatMap(Cons(10, Nil()))(v => Cons(v+1, Nil())))
    @Test
    def flatMapSimple() =
      val expected = Cons(11, Cons(21, Cons(31, Nil())))
      assertEquals(expected, flatMap(list)(v => Cons(v+1, Nil())))

    @Test
    def flatMapComplex() =
      val expected = Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil()))))))
      assertEquals(expected, flatMap(list)(v => Cons(v + 1, Cons(v + 2, Nil()))))

    @Test
    def mapSimple() =
      val expected = Cons(true, Cons(false, Cons(false, Nil())))
      assertEquals(expected, mapList(list)(v => v < 15))

    @Test
    def filterSimpleList() =
      val expected = Cons(10, Cons(20, Nil()))
      assertEquals(expected, filterList(list)(v => v < 25))

    @Test
    def maxComplexCase() =
      val expected = Some(25)
      assertEquals(expected, max(Cons(10, Cons(25, Cons(20, Nil())))))

    @Test
    def maxNoneCase() =
      val expected = None()
      assertEquals(expected, max(Nil()))

  class EdgeCaseTests:
    val expectedNil = Nil()
    @Test
    def dropIndexOutOfBounds() =
      val expected = Cons(10, Cons(20, Nil()))
      assertEquals(expected, drop(list, 5))

    @Test
    def dropNegativeIndex() =
      val expected = Cons(10, Cons(20, Nil()))
      assertEquals(expected, drop(list, -1))

    @Test
    def dropNil() =
      assertEquals(expectedNil, drop(Nil(), 0))
    @Test
    def appendNil() =
      assertEquals(expectedNil, append(Nil(), Nil()))

    @Test
    def flatMapNil() =
      assertEquals(expectedNil, flatMap(Nil[Int]())(el => Cons(el+1, Nil())))

    @Test
    def mapNil() =
      assertEquals(expectedNil, map(Nil())(el => Nil[Int]()))


class Part2MoreListTests:
  import u02.Modules.Person.*
  import u02.Modules.Person
  import u03.homework.Part1MoreOnLists.*


  val list = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

  @Test
  def getProfessorCourses() =
    val persons =
      Cons(Student("s1", 2022),
        Cons(Teacher("t1", "c1"),
          Cons(Teacher("t2", "c2"),
            Cons(Student("s2", 2023), Nil()))))

    val expected = Cons("c1", Cons("c2", Nil()))
    assertEquals(expected, getCourses(persons))

  @Test
  def foldListLeft() =
    assertEquals(-16, foldLeft(list)(0)(_ - _))

  @Test
  def foldListRight() =
    assertEquals(-8, foldRight(list)(0)(_ - _))

/* TODO part 3*/
class Part3Streams:

  def test() =
    assertEquals(0, 0)
