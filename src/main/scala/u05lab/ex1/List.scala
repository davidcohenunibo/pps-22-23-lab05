package u05lab.ex1

import u05lab.ex1.List

// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)
    case _ => None

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def append(list: List[A]): List[A] = this match
    case h :: t => h :: t.append(list)
    case _ => list

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def filter(predicate: A => Boolean): List[A] = this match
    case h :: t if predicate(h) => h :: t.filter(predicate)
    case _ :: t => t.filter(predicate)
    case _ => Nil()

  def map[B](fun: A => B): List[B] = this match
    case h :: t => fun(h) :: t.map(fun)
    case _ => Nil()

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight[List[B]](Nil())(f(_) append _)

  def foldLeft[B](z: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(z, h))(op)
    case Nil() => z

  def foldRight[B](z: B)(f: (A, B) => B): B = this match
    case h :: t => f(h, t.foldRight(z)(f))
    case _ => z

  def length: Int = foldLeft(0)((l, _) => l + 1)

  def isEmpty: Boolean = this match
    case Nil() => true
    case _ => false

  def reverse: List[A] = foldLeft[List[A]](Nil())((l, e) => e :: l)

  /** EXERCISES */

  def zipRight(i:Int): List[(A, Int)] = this match
    case h :: t => (h, i) :: t.zipRight(i+1)
    case _ => Nil()

  def partition(pred: A => Boolean): (List[A], List[A]) =
    foldRight((Nil(), Nil())) { case (e, (l1, l2)) =>
        if (pred(e)) (e :: l1, l2) else (l1, e :: l2)
    }

  def span(pred: A => Boolean): (List[A], List[A]) = {
    foldLeft((Nil(), Nil())) { case ((l1, l2), e) =>
      if (pred(e) && l2.isEmpty) (l1.append(List(e)), l2) else (l1, l2.append(List(e)))
    }
  }
  def reduce(op: (A, A) => A): A = this match
    case Nil() => throw UnsupportedOperationException()
    case h :: t => t.foldLeft(h)(op)

  def takeRight(n: Int): List[A] =
    foldRight(Nil()) { (e, acc) =>
      if (acc.length < n) e :: acc else acc
    }

  def collect[B](pf: PartialFunction[A, B]): List[B] =
    foldRight(Nil()){(e, list) =>
      if (pf.isDefinedAt(e)) pf(e) :: list else list
    }


// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)
