/*
 * Copyright (c) 2020. Zhen Zhang
 */

package com.research.nomad.markii

import java.io.File

import presto.android.Configs
import soot.{Scene, SootClass, SootMethod}
import soot.jimple.infoflow.android.iccta.{Ic3ResultLoader, Intent}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

object Ic3Manager {
  private val ic3 = Configs.project.replace(".apk", "_ic3.txt")
  var ic3Enabled: Boolean = false
  val methodIc3Map: mutable.Map[SootMethod, mutable.Set[(SootClass, Set[Intent])]] = mutable.Map()

  def init() {
    if (new File(ic3).exists) {
      ic3Enabled = true
      val app = Ic3ResultLoader.load(ic3)
      if (app != null) {
        for (component <- app.getComponentList.asScala) {
          val src = Scene.v.getSootClass(component.getName)
          for (p <- component.getExitPointsList.asScala) {
            val exitMethod = Scene.v.getMethod(p.getInstruction.getMethod)
            val intents = app.getIntents(component, p)
            if (intents != null && intents.size() > 0) {
              methodIc3Map.getOrElseUpdate(exitMethod, mutable.Set()).add((src, intents.asScala.toSet))
            }
          }
        }
      }
    }
  }
}
