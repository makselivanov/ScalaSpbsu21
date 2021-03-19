package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._
import scala.annotation.tailrec

sealed trait MyGenericList[+A] {
  def head: A
  def tail: MyGenericList[A]
  def drop(n: Int): MyGenericList[A]
  def take(n: Int): MyGenericList[A]
  def map[B](f: A => B): MyGenericList[B]
  def ::[B >: A](elem: B): MyGenericList[B] = MyGenericCons(elem, this)
  ///I don't get it, why just type 'A' don't work here
}

case object MyNil extends MyGenericList[Nothing] {
  override lazy val head: Nothing = undef
  override lazy val tail: MyGenericList[Nothing] = undef
  override def drop(n: Int): MyGenericList[Nothing] = if (n == 0) MyNil else undef
  override def take(n: Int): MyGenericList[Nothing] = if (n == 0) MyNil else undef
  override def map[B](f: Nothing => B): MyGenericList[Nothing] = MyNil
}

final case class MyGenericCons[A](override val head: A, override val tail: MyGenericList[A]) extends MyGenericList[A] {
  override def drop(n: Int): MyGenericList[A] = n match {
    case n if n > 0 => tail.drop(n - 1)
    case 0 => this
    case _ => undef
  }
  override def take(n: Int): MyGenericList[A] = n match {
    case n if n > 0 => head :: tail.take(n - 1)
    case 0 => MyNil
    case _ => undef
  }
  override def map[B](f: A => B): MyGenericList[B] = f(head) :: tail.map(f)
}

object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq[A](seq: Seq[A]): MyGenericList[A] = if (seq.isEmpty) MyNil else seq.head :: fromSeq(seq.tail)
  def sum[A](list: MyGenericList[A]):  Int      = list match {
    case MyNil => undef
    case _     => foldLeft(list, 0) { (x, r) => x.asInstanceOf[Int] + r }
  }
  def size[A](list: MyGenericList[A]): Int      = foldLeft(list, 0) { (_, r) => r + 1}

  @tailrec
  def foldLeft[A, B](list: MyGenericList[A], z: B)(op: (A, B) => B): B = if (list == MyNil) z else
    foldLeft(list.tail, op(list.head, z))(op)
}
