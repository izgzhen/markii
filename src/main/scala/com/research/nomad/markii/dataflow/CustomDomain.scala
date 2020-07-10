/*
 * Copyright (c) 2020. Zhen Zhang
 */

package com.research.nomad.markii.dataflow

import com.research.nomad.markii.PreAnalyses
import soot.jimple.{InstanceInvokeExpr, InvokeExpr, Stmt}
import soot.{Local, SootClass, SootMethod}

trait CustomObjectStateTransformer[S] {
  def initInstance(sootClass: SootClass): Option[S]
  def updatedInstance(s: S, instanceInvokeExpr: InstanceInvokeExpr): S
}

/**
 * TODO
 */
case class CustomDomain[S](private val localMap: Map[Local, AccessPath[S]],
                           private val globalMap: Map[SootClass, AccessPath[S]]) {
  /**
   * STRUCTURAL METHOD
   */
  def meet(d: CustomDomain[S]): CustomDomain[S] = {
    if (equalsTop) return d
    if (d.equalsTop) return this
    if (equals(d)) return this
    CustomDomain(
      CustomDomain.mergeMapOfAccessPath(localMap, d.localMap),
      CustomDomain.mergeMapOfAccessPath(globalMap, d.globalMap))
  }

  def equalsTop: Boolean = localMap.isEmpty && globalMap.isEmpty

  /**
   * STRUCTURAL METHOD
   */
  def updateVal(contextMethod: SootMethod, stmt: Stmt, local: Local,
                valMapper: S => S): CustomDomain[S] = {
    copy(localMap = localMap.map { case (l, accessPath) =>
      if (PreAnalyses.isAlias(local, l, stmt, stmt, contextMethod)) {
        (l, accessPath.updateData(valMapper))
      } else {
        (l, accessPath)
      }
    })
  }

  /**
   * STRUCTURAL METHOD
   */
  def sizeSummary: Map[String, Int] =
    Map(
      "localMap" -> localMap.size,
      "globalMap" -> globalMap.size)

  /**
   * STRUCTURAL METHOD
   */
  def getHeap: CustomDomain[S] = copy(localMap = Map())

  /**
   * STRUCTURAL METHOD
   */
  def nonEmpty: Boolean =
    localMap.nonEmpty || globalMap.nonEmpty

  def newInstance(transformer: CustomObjectStateTransformer[S], ref: Ref,
                  sootClass: SootClass, stmt: Stmt): CustomDomain[S] = {
    transformer.initInstance(sootClass) match {
      case Some(v) => withVal(ref, v)
      case None => this
    }
  }

  def invokeMethod(transformer: CustomObjectStateTransformer[S], ctxMethod: SootMethod,
                   invokeExpr: InvokeExpr, stmt: Stmt): CustomDomain[S] = {
    invokeExpr match {
      case instanceInvokeExpr: InstanceInvokeExpr =>
        updateVal(ctxMethod, stmt, instanceInvokeExpr.getBase.asInstanceOf[Local],
          v => transformer.updatedInstance(v, instanceInvokeExpr))
      case _ => this
    }
  }

  /**
   * STRUCTURAL METHOD
   */
  private def equivTo(domain: CustomDomain[S]): Boolean =
    localMap == domain.localMap && globalMap == domain.globalMap

  override def equals(obj: Any): Boolean = {
    if (obj == null) return false
    if (super.equals(obj)) return true
    obj match {
      case domain: CustomDomain[S] =>
        if (hashCode() == domain.hashCode()) {
          equivTo(domain)
        } else {
          false
        }
      case _ => false
    }
  }

  override def toString: String = sizeSummary.toString

  def getLocalVals(contextMethod: SootMethod, stmt: Stmt, local: Local): Iterable[S] = {
    localMap.flatMap { case (l, accessPath) =>
      if (PreAnalyses.isAlias(local, l, stmt, stmt, contextMethod)) {
        accessPath.data.getOrElse(Set())
      } else {
        Set()
      }
    }
  }

  def getAccessPath(l: Local): Option[AccessPath[S]] = localMap.get(l)

  def getAccessPath(ref: Ref): Option[AccessPath[S]] =
    ref match {
      case Ref.GlobalRef(cls, fields) =>
        globalMap.get(cls) match {
          case Some(accessPath) => accessPath.get(fields)
          case None => None
        }
      case Ref.LocalRef(local, fields) =>
        localMap.get(local) match {
          case Some(accessPath) => accessPath.get(fields)
          case None => None
        }
    }

  def withSubPath(ref: Ref, subPath: AccessPath[S]): CustomDomain[S] = {
    ref match {
      case Ref.GlobalRef(cls, fields) =>
        copy(
          globalMap = globalMap.get(cls) match {
            case None => globalMap + (cls -> AccessPath(None).add(fields, subPath))
            case Some(accessPath) => globalMap + (cls -> accessPath.add(fields, subPath))
          }
        )
      case Ref.LocalRef(local, fields) =>
        copy(
          localMap = localMap.get(local) match {
            case None => localMap + (local -> AccessPath(None).add(fields, subPath))
            case Some(accessPath) => localMap + (local -> accessPath.add(fields, subPath))
          }
        )
    }
  }

  def killRef(ref: Ref): CustomDomain[S] =
    ref match {
      case Ref.GlobalRef(cls, fields) =>
        copy(globalMap = (fields, globalMap.get(cls)) match {
          case (_, None) => globalMap
          case (List(), _) => globalMap - cls
          case (fields, Some(accessPath)) => globalMap + (cls -> accessPath.kill(fields))
        })
      case Ref.LocalRef(local, fields) =>
        copy(localMap = (fields, localMap.get(local)) match {
          case (_, None) => localMap
          case (List(), _) => localMap - local
          case (fields, Some(accessPath)) => localMap + (local -> accessPath.kill(fields))
        })
    }

  def withVal(ref: Ref, v: S): CustomDomain[S] = {
    withVals(ref, Set(v))
  }

  def withVals(ref: Ref, vals: Set[S]): CustomDomain[S] = {
    ref match {
      case Ref.GlobalRef(cls, fields) =>
        copy(
          globalMap = if (vals.nonEmpty) {
            globalMap + (cls -> AccessPath(None).add(fields, vals))
          } else {
            globalMap
          }
        )
      case Ref.LocalRef(local, fields) =>
        copy(
          localMap = if (vals.nonEmpty) {
            localMap + (local -> AccessPath(None).add(fields, vals))
          } else {
            localMap
          }
        )
    }
  }

  def toJSONObj: Object = Map[String, Object](
    "localMap" -> localMap.map { case (k, v) => (k.toString(), v.toJSONObj) },
    "globalMap" -> globalMap.map { case (k, v) => (k.toString(), v.toJSONObj) }
  )
}

object CustomDomain {
  def makeTop[V]: CustomDomain[V] = CustomDomain[V](Map(), Map())

  def mapToString[K, V](m: Map[K, V]): String =
    m.map { case (k, v) => "\t " + k + " -> " + v }.mkString("\n")

  def mergeMapOfSets[K, V](m1: Map[K, Set[V]], m2: Map[K, Set[V]]): Map[K, Set[V]] = {
    if (m1.equals(m2)) return m1
    mergeMaps(m1, m2, (m1: Set[V], m2: Set[V]) => m1 ++ m2)
  }

  def mergeMaps[K, V](m1: Map[K, V], m2: Map[K, V], f: (V, V) => V): Map[K, V] = {
    (m1.keySet ++ m2.keySet).map(k => {
      if (m1.contains(k) && m2.contains(k)) {
        (k, f(m1(k), m2(k)))
      } else if (m1.contains(k)) {
        (k, m1(k))
      } else {
        (k, m2(k))
      }
    }).toMap
  }

  def mergeMapOfAccessPath[K, D](m1: Map[K, AccessPath[D]],
                                 m2: Map[K, AccessPath[D]]): Map[K, AccessPath[D]] = {
    if (m1.equals(m2)) return m1
    mergeMaps(m1, m2, (m1: AccessPath[D], m2: AccessPath[D]) => m1.merge(m2))
  }
}