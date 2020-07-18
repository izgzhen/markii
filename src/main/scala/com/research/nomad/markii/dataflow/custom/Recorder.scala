/*
 * Copyright (c) 2020. Zhen Zhang
 */

package com.research.nomad.markii.dataflow.custom

import com.research.nomad.markii.dataflow.{AbsValSet, CustomObjectStateTransformer}
import soot.{SootClass, SootMethod}
import soot.jimple.{InstanceInvokeExpr, Stmt}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

object RecorderState extends Enumeration {
  val Started, Stopped = Value
}

object Recorder extends CustomObjectStateTransformer[AbsValSet[RecorderState.Value]] {
  type D = AbsValSet[RecorderState.Value]

  override def initInstance(sootClass: SootClass): Option[D] = {
    if (sootClass.getName == "android.media.MediaRecorder") {
      Some(AbsValSet(Set(RecorderState.Stopped)))
    } else {
      None
    }
  }

  /**
   * Key: Statement
   * Value: Set of possible transitions (prev-state, post-state)
   */
  private val transitionSites = mutable.Map[Stmt, mutable.Set[(RecorderState.Value, RecorderState.Value)]]()

  def getTransitions(m: SootMethod): Set[(RecorderState.Value, RecorderState.Value)] = {
    m.getActiveBody.getUnits.asScala.flatMap(u => transitionSites.getOrElse(u.asInstanceOf[Stmt], Set())).toSet
  }

  override def updatedInstance(prevVals: D, instanceInvokeExpr: InstanceInvokeExpr, callSite: Stmt): D = {
    if (instanceInvokeExpr.getMethod.getSignature == "<android.media.MediaRecorder: void start()>") {
      for (prevVal <- prevVals.vals) {
        transitionSites.getOrElseUpdate(callSite, mutable.Set()).add((prevVal, RecorderState.Started))
      }
      AbsValSet(Set(RecorderState.Started))
    } else if (instanceInvokeExpr.getMethod.getSignature == "<android.media.MediaRecorder: void stop()>") {
      for (prevVal <- prevVals.vals) {
        transitionSites.getOrElseUpdate(callSite, mutable.Set()).add(prevVal, RecorderState.Stopped)
      }
      AbsValSet(Set(RecorderState.Stopped))
    } else {
      prevVals
    }
  }
}
