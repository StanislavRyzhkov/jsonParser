package company.ryzhkov

import scala.collection.mutable.ArrayBuffer

sealed trait Tree

object Tree {
  sealed abstract class StructuredTree(
    val elements: ArrayBuffer[Tree] = ArrayBuffer.empty,
    var parent:   Option[StructuredTree] = None
  ) extends Tree {
    def addElement(element:  Tree): Unit           = elements += element
    def setParent(newParent: StructuredTree): Unit = parent = Some(newParent)
  }

  case class NumberLeaf(value: Int) extends Tree
  case class StringLeaf(value: String) extends Tree
  case class BooleanLeaf(value: Boolean) extends Tree

  case object NullLeaf extends Tree

  class ListLeaf extends StructuredTree() {
    override def toString = s"ListLeaf($elements)"
  }

  class ObjectLeaf extends StructuredTree() {
    override def toString = s"ObjectLeaf($elements)"
  }
}
