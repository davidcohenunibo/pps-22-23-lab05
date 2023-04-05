package u05lab.ex3

import java.util.concurrent.TimeUnit
import scala.collection.immutable.ListSet
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.util.Random

object PerformanceUtils:
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]]:
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] =
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
    if (msg.nonEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)

@main def checkPerformance(): Unit =
  import PerformanceUtils.*
  val random = new Random()
  val sizes = Seq(1000000)

  // Seq
  println("\n---SEQ---")
  sizes.foreach(size => {
    val seq = 1 to size
    println(s"\nSeq - size $size")
    measure(s"query first")(seq.head)
    measure(s"query last")(seq.last)
    measure(s"query middle")(seq(size / 2))
    measure(s"append")(seq :+ size)
    measure(s"delete last")(seq.init)
    measure(s"delete first")(seq.tail)
    measure(s"delete middle")(seq.take(size / 2) ++ seq.drop(size / 2 + 1))
    measure(s"map")(seq.map(_ + 1))
    measure(s"filter")(seq.filter(_ % 2 == 0))
  })

  //LinearSeq
  println("\n---LINEARSEQ---")

  // List
  sizes.foreach { size =>
    val list = (1 to size).toList
    val listbuffer = scala.collection.mutable.ListBuffer.from(list)
    println(s"\nList - size $size")
    measure(s"list - query first")(list.head)
    measure(s"listbuffer - query first")(listbuffer.head)
    measure(s"list  - query last")(list.last)
    measure(s"listbuffer - query last")(listbuffer.last)
    measure(s"list - query middle")(list(size / 2))
    measure(s"listbuffer - query middle")(listbuffer(size / 2))
    measure(s"list - append")(list :+ size)
    measure(s"listbuffer - append")(listbuffer += size)
    measure(s"list - delete last")(list.init)
    measure(s"listbuffer - delete last")(listbuffer.remove(size - 1))
  }

  //Queue
  sizes.foreach { size =>
    val queue = scala.collection.immutable.Queue.tabulate(size)(i => random.nextInt())
    val mutQueue = mutable.Queue.from(queue)
    println(s"\n->Queue - size $size")
    measure(s"queue - query first")(queue.head)
    measure(s"mutQueue - query first")(mutQueue.head)
    measure(s"queue - query last")(queue.last)
    measure(s"mutQueue - query last")(mutQueue.last)
    measure(s"queue - query middle")(queue(size / 2))
    measure(s"mutQueue - query middle")(mutQueue(size / 2))
    measure(s"queue - add new")(queue.enqueue(random.nextInt()))
    measure(s"mutQueue - add new")(mutQueue += random.nextInt())
    measure(s"queue - remove last")(queue.dequeue._2)
    measure(s"mutQueue - remove last")(mutQueue.dequeue())
  }
  // IndexedSeq
  println("\n---INDEXEDSEQ---")

  // HashSet
  sizes.foreach { size =>
    val immSet = Set.tabulate(size)(_ => random.nextInt())
    val mutSet = mutable.Set.from(immSet)
    println(s"\n->Set - size $size")
    measure(s"imm - query first")(immSet.head)
    measure(s"mut - query first")(mutSet.head)
    measure(s"imm - query last")(immSet.max)
    measure(s"mut - query last")(mutSet.max)
    measure(s"imm - query middle")(immSet((size / 2) - 1))
    measure(s"mut - query middle")(mutSet((size / 2) - 1))
    measure(s"imm - add new")(immSet + random.nextInt())
    measure(s"mut - add new")(mutSet += random.nextInt())
    measure(s"imm - remove last")(immSet - immSet.max)
    measure(s"mut - remove last")(mutSet -= mutSet.max)
    measure(s"imm - add all")(immSet ++ Seq.tabulate(size)(_ => random.nextInt()))
    measure(s"mut - add all")(mutSet ++= Seq.tabulate(size)(_ => random.nextInt()))
  }

  //Map
  println("\n---MAP---")

  // HashMap
  sizes.foreach { size =>
    val immMap = Map(Seq.tabulate(size)(i => i -> random.nextInt()): _*)
    val mutMap = mutable.Map.from(immMap)
    println(s"\n->Map - size $size")
    measure(s"imm - query first")(immMap.head)
    measure(s"mut - query first")(mutMap.head)
    measure(s"imm - query last")(immMap(size - 1))
    measure(s"mut - query last")(mutMap(size - 1))
    measure(s"imm - query middle")(immMap(size / 2))
    measure(s"mut - query middle")(mutMap(size / 2))
    measure(s"imm - add new")(immMap + (size -> random.nextInt()))
    measure(s"mut - add new")(mutMap += (size -> random.nextInt()))
    measure(s"imm - remove last")(immMap - (size - 1))
    measure(s"mut - remove last")(mutMap.remove(size - 1))
    measure(s"imm - add all")(Map(Seq.tabulate(size)(i => (size + i) -> random.nextInt()): _*) ++ immMap)
    measure(s"mut - add all")(mutMap ++= Seq.tabulate(size)(i => (size + i) -> random.nextInt()))
  }

  // Some immutables collections
  println("\n---SOME IMMUTABLES---")
  // Vector
  sizes.foreach { size =>
    val vector = Vector.tabulate(size)(i => random.nextInt())
    println(s"\n->Vector - size $size")
    measure(s"query first")(vector.head)
    measure(s"query last")(vector.last)
    measure(s"query middle")(vector(size / 2))
    measure(s"append")(vector :+ random.nextInt())
    measure(s"delete last")(vector.init)
    measure(s"delete first")(vector.tail)
    measure(s"delete middle")(vector.take(size / 2) ++ vector.drop(size / 2 + 1))
    measure(s"map")(vector.map(_ + 1))
    measure(s"filter")(vector.filter(_ % 2 == 0))
  }
  // Range
  sizes.foreach { size =>
    val range = 0 until size
    println(s"\n->Range - size $size")
    measure(s"query first")(range.head)
    measure(s"query last")(range.last)
    measure(s"query middle")(range(size / 2))
    measure(s"append")(range :+ size)
    measure(s"delete last")(range.init)
    measure(s"delete first")(range.tail)
    measure(s"delete middle")(range.take(size / 2) ++ range.drop(size / 2 + 1))
    measure(s"map")(range.map(_ + 1))
    measure(s"filter")(range.filter(_ % 2 == 0))
  }
  // String
  sizes.foreach { size =>
    val string = "a" * size
    println(s"\n->String - size $size")
    measure(s"query first")(string.head)
    measure(s"query last")(string.last)
    measure(s"query middle")(string(size / 2))
    measure(s"append")("a" + string)
    measure(s"delete last")(string.init)
    measure(s"delete first")(string.tail)
    measure(s"delete middle")(string.take(size / 2) ++ string.drop(size / 2 + 1))
    measure(s"map")(string.map(_ + 1))
    measure(s"filter")(string.filter(_ % 2 == 0))
  }

  // Some mutable collections
  println("\n---SOME MUTABLES---")

  // Mutable Stack
  sizes.foreach { size =>
    val mutStack = mutable.Stack.tabulate(size)(i => random.nextInt())
    println(s"\nMutable Stack - size $size")
    measure(s"stack - query first")(mutStack.head)
    measure(s"stack - query last")(mutStack.last)
    measure(s"stack - query middle")(mutStack(size / 2))
    measure(s"stack - add new")(mutStack.push(random.nextInt()))
    measure(s"stack - remove last")(mutStack.pop())
  }

//  // Mutable Deque
  sizes.foreach { size =>
    val mutDeque = mutable.ArrayDeque.tabulate(size)(i => random.nextInt())
    println(s"\nMutable Deque - size $size")
    measure(s"deque - query first")(mutDeque.head)
    measure(s"deque - query last")(mutDeque.last)
    measure(s"deque - query middle")(mutDeque(size / 2))
    measure(s"deque - add new")(mutDeque += random.nextInt())
    measure(s"deque - remove last")(mutDeque.removeLast())
  }




