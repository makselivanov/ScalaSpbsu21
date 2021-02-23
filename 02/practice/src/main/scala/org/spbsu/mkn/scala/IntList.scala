package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

import scala.annotation.tailrec

sealed trait IntList {
  def head: Int
  def tail: IntList
  def drop(n: Int): IntList
  def take(n: Int): IntList
  def map(f: Int => Int): IntList
  def ::(elem: Int): IntList = new IntCons(elem, this)
}

final class IntCons(override val head: Int, override val tail: IntList) extends IntList {
  override def drop(n: Int): IntList = n match {
    case n if n > 0 => tail.drop(n - 1)
    case 0 => this
    case _ => undef
  }
  override def take(n: Int): IntList = n match {
    case n if n > 0 => head :: tail.take(n - 1)
    case 0 => IntNil
    case _ => undef
  }
  override def map(f: Int => Int): IntList = f(head) :: tail.map(f)

  override def equals(obj: Any): Boolean = obj match {
    case obj: IntCons =>
      obj.head == obj.head && obj.tail == obj.tail
    case _ => false
  }
}

object IntNil extends IntList {
  override lazy val head: Int = undef
  override lazy val tail: IntList = undef
  override def drop(n: Int): IntList = if (n == 0) IntNil else undef
  override def take(n: Int): IntList = if (n == 0) IntNil else undef
  override def map(f: Int => Int): IntList = IntNil
}

object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq(seq: Seq[Int]): IntList = if (seq.isEmpty) IntNil else seq.head :: fromSeq(seq.tail)
  def sum(intList: IntList): Int      = foldLeft(intList, 0)(_+_)
  def size(intList: IntList): Int     = foldLeft(intList, 0)((r, _) => r + 1)

  @tailrec
  def foldLeft(intList: IntList, z: Int)(op: (Int, Int) => Int): Int = if (intList == IntNil) z else
    foldLeft(intList.tail, op(z, intList.head))(op)
}