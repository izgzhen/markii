/*
 * Copyright (c) 2020. Zhen Zhang
 */

package com.research.nomad.markii

import java.io.PrintWriter

import soot.{SootMethod, UnitPatchingChain}
import soot.jimple.{InvokeExpr, Stmt}
import vasco.DataFlowSolution

import scala.collection.mutable
import scala.jdk.CollectionConverters._

object Util {
  /**
   * getMethod will throw exception sometimes...
   *
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

  def getUnitIndex(units: UnitPatchingChain, u: soot.Unit): Int = {
    units.asScala.view.toSeq.indexOf(u)
  }

  def dumpVASCOAbstractions[D](outputPath: String, solution: DataFlowSolution[soot.Unit, D],
                               nonEmpty: D => Boolean,
                               domainToString: D => String,
                               methods: Iterable[SootMethod]): Unit = {
    val printWriter = new PrintWriter(outputPath)
    for (m <- methods) {
      printWriter.println("====== Method " + m.getSignature + " =======")
      printWriter.println(m.getActiveBody)
      for (unit <- m.getActiveBody.getUnits.asScala) {
        val d = solution.getValueAfter(unit)
        if (d != null && nonEmpty(d)) {
          printWriter.println("\tUnit: " + unit)
          printWriter.println("Domain: ")
          printWriter.println(d, domainToString(d))
          printWriter.println()
        }
      }
    }
    printWriter.close()
  }

  def getJavaLineNumber(sootMethod : SootMethod): Int = {
    for (unit <- sootMethod.getActiveBody.getUnits.asScala) {
      val tmp = unit.getJavaSourceStartLineNumber
      if (tmp != -1){
        return tmp
      }
    }
    -1
  }

  private val stmtOffsets: mutable.Map[SootMethod, Map[Stmt, Int]] = mutable.Map()
  def stmtOffset(method: SootMethod, stmt: Stmt): Int = {
    if (!stmtOffsets.contains(method)) {
      stmtOffsets.put(method, method.getActiveBody.getUnits.iterator().asScala.map(_.asInstanceOf[Stmt]).toList.zipWithIndex.toMap)
    }
    stmtOffsets(method)(stmt)
  }

  private val stmtIds: mutable.Map[Stmt, Int] = mutable.Map()
  def stmtId(stmt: Stmt): Int = {
    if (stmtIds.contains(stmt)) {
      stmtIds(stmt)
    } else {
      val m = GUIAnalysis.getMethodOf(stmt)
      if (m == null) {
        return stmt.hashCode()
      }
      val i = m.getSignature.hashCode ^ stmtOffset(m, stmt) + 1
      stmtIds.put(stmt, i)
      i
    }
  }
}
