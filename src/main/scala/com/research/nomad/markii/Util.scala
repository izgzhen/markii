/*
 * Copyright (c) 2020. Zhen Zhang
 */

package com.research.nomad.markii

import soot.SootMethod
import soot.jimple.{InvokeExpr, Stmt}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

object Util {
  /**
   * getMethod will throw exception sometimes...
   *
   * FIXME: upgrade Soot
   * @param invokeExpr
   * @return
   */
  def getMethodUnsafe(invokeExpr: InvokeExpr): SootMethod = {
    try {
      invokeExpr.getMethod
    } catch {
      case ignored: Exception =>
        println(ignored)
        null
    }
  }

  def locateStmt(sootMethod: SootMethod, stmt: Stmt): String = {
    val lines = mutable.ArrayBuffer[String]()

    for (unit <- sootMethod.getActiveBody.getUnits.asScala) {
      if (unit != stmt) {
        lines.addOne(unit.toString)
      } else {
        lines.addOne("************************")
        lines.addOne(unit.toString)
        lines.addOne("************************")
      }
    }
    lines.mkString("\n")
  }
}
