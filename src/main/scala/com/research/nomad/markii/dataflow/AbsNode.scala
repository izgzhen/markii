/*
 * Copyright (c) 2020. Zhen Zhang
 */

package com.research.nomad.markii.dataflow

import presto.android.gui.listener.EventType
import presto.android.xml.{AndroidView, MethodInfo}
import soot.SootClass
import soot.jimple.Stmt

import scala.collection.mutable
import scala.jdk.CollectionConverters._

sealed abstract class AbsAttr extends Product with Serializable

object AbsAttr {
  final case class IntVal(i: Int) extends AbsAttr
  final case class StrVal(s: String) extends AbsAttr
  final case class ClassVal(c: SootClass) extends AbsAttr
}

/**
 * Node in Abstract Flow Tree
 */
sealed abstract class AbsNode extends Product with Serializable

object AbsNode {
  /**
   * Node for a view element
   * @param allocSite: Allocation statement
   * @param id: Possible resource ids
   * @param sootClass: Associated class, e.g. android.widget.Button
   * @param attributes: Attribute values
   * @param androidView: Associated AndroidView from parsed XML
   */
  final case class ViewNode(allocSite: Stmt,
                            id: Set[Int] = Set(),
                            sootClass: Option[SootClass] = None,
                            attributes: Map[AndroidView.ViewAttr, Set[String]] = Map(),
                            private val androidView: AndroidView) extends AbsNode {

    // FIXME: merge attributes without changing the hash code?
    //        or maybe factoring out the attribute map into another abstract state?
    def setAttributes(attrs: Set[(AndroidView.ViewAttr, String)]): AbsNode = {
      val newAttrs = mutable.Map[AndroidView.ViewAttr, Set[String]]()
      for ((attr, value) <- attrs) {
        newAttrs.put(attr, newAttrs.getOrElse(attr, Set()) + value)
      }
      for ((attr, values) <- attributes) {
        if (!newAttrs.contains(attr)) {
          newAttrs.put(attr, values)
        }
      }
      copy(attributes = newAttrs.toMap)
    }

    def nodeID: Int = hashCode()

    override def toString: String = f"ViewNode(${nodeID.toString}, id=${id}, class=${sootClass})"

    def getAttrs: Iterable[(AndroidView.ViewAttr, String)] = {
      val defaultAttrs = if (androidView != null) {
        androidView.getViewAttrs.asScala.filter { case (attr, _) => !attributes.contains(attr) }
      } else {
        List()
      }
      defaultAttrs ++ attributes.flatMap { case (attr, values) => values.map((attr, _)) }
    }

    def getAppAttrs: Iterable[(AndroidView.ViewAppAttr, String)] = if (androidView != null) {
      androidView.getViewAppAttrs.asScala
    } else {
      List()
    }

    def getInlineClickHandlers: Iterable[(EventType, MethodInfo)] = if (androidView != null) {
      androidView.getInlineClickHandlers.asScala
    } else {
      List()
    }
  }

  final case class ActNode(sootClass: SootClass) extends AbsNode

  final case class ListenerNode(listener: SootClass) extends AbsNode

  final case class LayoutParamsNode(attrs: Set[(AndroidView.ViewAttr, String)] = Set()) extends AbsNode

  final case class UnifiedObjectNode(absName: String, absAttrs: Map[String, AbsAttr]) extends AbsNode
}