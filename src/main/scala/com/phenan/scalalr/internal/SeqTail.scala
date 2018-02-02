package com.phenan.scalalr.internal

sealed trait SeqTail [+T] {
  def toSeq: Seq[T]
}

object SeqTail {
  def empty[T]: SeqTail [T] = NilSeqTail
}

case class ConsSeqTail [+T] (head: T, tail: SeqTail[T]) extends SeqTail[T] {
  override def toSeq: Seq[T] = head +: tail.toSeq
}

case object NilSeqTail extends SeqTail[Nothing] {
  override def toSeq: Seq[Nothing] = Nil
}
