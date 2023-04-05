package u05lab.ex1

import org.junit.Test
import org.junit.Assert.*

class ListTest:

  val listOfInteger: List[Int] = List(1, 2, 3, 4)

  val listOfChar: List[Int] = List('a', 'b', 'c', 'd', 'e')

  @Test
  def testZipRight(): Unit =
    assertEquals(List((1,0),(2,1),(3,2),(4,3)),listOfInteger.zipRight(0))
  assertEquals(List((1,4),(2,5),(3,6),(4,7)),listOfInteger.zipRight(4))

  @Test
  def testPartition(): Unit =
    assertEquals((List(2, 4), List(1, 3)),listOfInteger.partition(_ % 2 == 0))

  @Test
  def testSpan(): Unit =
    assertEquals((List(1), List(2, 3, 4)), listOfInteger.span(_ % 2 != 0))
    assertEquals((List(1,2), List( 3, 4)), listOfInteger.span(_ < 3))

  @Test
  def testReduce(): Unit =
    assertThrows(classOf[UnsupportedOperationException], () => List.Nil[Int]().reduce(_ + _))
    assertEquals(10, listOfInteger.reduce(_ + _))
    assertEquals(24, listOfInteger.reduce(_ * _))
    assertEquals(-8, listOfInteger.reduce(_ - _))

  @Test
  def testTakeRight(): Unit =
    assertEquals(List(4), listOfInteger.takeRight(1))
    assertEquals(List(3, 4), listOfInteger.takeRight(2))
    assertEquals(List(1, 2, 3, 4), listOfInteger.takeRight(5))
    assertEquals(List.Nil(), listOfInteger.takeRight(0))

  @Test
  def testCollect(): Unit =
    assertEquals(List(2,4), listOfInteger.collect({ case x if x % 2 == 0 => x }))
    assertEquals(List(1,3), listOfInteger.collect({ case x if x % 2 != 0 => x }))
    assertEquals(List.Nil(), listOfInteger.collect({ case x if x > 10 => x }))
    assertEquals(List.Nil(), List.Nil[Int]().collect({ case x if x > 10 => x }))
    assertEquals(List(1,2,3,4), listOfInteger.collect({ case x => x }))




