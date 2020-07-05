/*
 * Copyright (c) 2020. Zhen Zhang
 */

package com.research.nomad.markii

import soot.SootMethod
import soot.jimple.InvokeExpr

object Util {
  def getMethodUnsafe(invokeExpr: InvokeExpr): SootMethod = {
    try {
      invokeExpr.getMethod
    } catch {
      case ignored: Exception =>
        println(ignored)
        null
    }
  }
}
