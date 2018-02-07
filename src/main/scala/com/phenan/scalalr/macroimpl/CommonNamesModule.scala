package com.phenan.scalalr.macroimpl

trait CommonNamesModule {
  this: MacroModule =>

  import c.universe._

  def getNilListOf (componentTypeTree: Tree): Tree = q"scala.collection.immutable.List.empty[$componentTypeTree]"
  def makeSingleElementList (componentTypeTree: Tree, arg: Tree): Tree = q"scala.collection.immutable.List[$componentTypeTree]($arg)"
  def makeSingleElementSeqTail (componentTypeTree: Tree, arg: Tree): Tree = q"com.phenan.scalalr.internal.ConsSeqTail[$componentTypeTree]($arg, com.phenan.scalalr.internal.NilSeqTail)"

  def consSeq (left: Tree, right: Tree): Tree = q"$left +: $right"
  def consSeqTail (left: Tree, right: Tree): Tree = q"com.phenan.scalalr.internal.ConsSeqTail($left, $right)"

  def seqTailToSeq (s: Tree): Tree = q"$s.toSeq"

  def seqTypeTreeOf (componentTypeTree: Tree): Tree = tq"scala.collection.Seq[$componentTypeTree]"
  def seqTailTypeTreeOf (componentTypeTree: Tree): Tree = tq"com.phenan.scalalr.internal.SeqTail[$componentTypeTree]"

  lazy val syntaxAnnotationType: Type = c.typecheck(tq"com.phenan.scalalr.syntax", c.TYPEmode).tpe
  lazy val sepAnnotationType: Type = c.typecheck(tq"com.phenan.scalalr.sep", c.TYPEmode).tpe
}
