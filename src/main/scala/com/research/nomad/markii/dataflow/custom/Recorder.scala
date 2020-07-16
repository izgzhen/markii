/*
 * Copyright (c) 2020. Zhen Zhang
 */

package com.research.nomad.markii.dataflow.custom

import com.research.nomad.markii.dataflow.{AbsValSet, CustomObjectStateTransformer, Ref}
import soot.{SootClass, SootMethod}
import soot.jimple.{InstanceInvokeExpr, InvokeExpr, Stmt}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

sealed abstract class RecorderAbsValue extends Product with Serializable

object RecorderState extends Enumeration {
  val Started, Stopped = Value
}

object RecorderAbsValue {
  final case class MediaRecorder(s: RecorderState.Value) extends RecorderAbsValue
  final case class Activity(sootClass: SootClass) extends RecorderAbsValue
}


object Recorder extends CustomObjectStateTransformer[AbsValSet[RecorderAbsValue]] {
  type D = AbsValSet[RecorderAbsValue]
  override def initInstance(sootClass: SootClass): Option[D] = {
    if (sootClass.getName == "android.media.MediaRecorder") {
      Some(AbsValSet(Set(RecorderAbsValue.MediaRecorder(s = RecorderState.Stopped))))
    } else {
      None
    }
  }

  /**
   * Key: prev-state, post-state
   * Value: Set of statements
   */
  private val transitionSites = mutable.Map[Stmt, mutable.Set[(RecorderState.Value, RecorderState.Value)]]()

  def getTransitions(m: SootMethod): Set[(RecorderState.Value, RecorderState.Value)] = {
    m.getActiveBody.getUnits.asScala.flatMap(u => transitionSites.getOrElse(u.asInstanceOf[Stmt], Set())).toSet
  }

  override def updatedInstance(prevVal: D, instanceInvokeExpr: InstanceInvokeExpr, callSite: Stmt): D = {
    if (instanceInvokeExpr.getMethod.getSignature == "<android.media.MediaRecorder: void start()>") {
      for (prevVal <- prevVal.vals) {
        prevVal match {
          case RecorderAbsValue.MediaRecorder(isRecording) =>
            transitionSites.getOrElseUpdate(callSite, mutable.Set()).add(isRecording, RecorderState.Started)
          case RecorderAbsValue.Activity(_) =>
        }
      }
      AbsValSet(Set(RecorderAbsValue.MediaRecorder(s = RecorderState.Started)))
    } else if (instanceInvokeExpr.getMethod.getSignature == "<android.media.MediaRecorder: void stop()>") {
      for (prevVal <- prevVal.vals) {
        prevVal match {
          case RecorderAbsValue.MediaRecorder(isRecording) =>
            transitionSites.getOrElseUpdate(callSite, mutable.Set()).add(isRecording, RecorderState.Stopped)
          case RecorderAbsValue.Activity(_) =>
        }
      }
      AbsValSet(Set(RecorderAbsValue.MediaRecorder(s = RecorderState.Stopped)))
    } else {
      prevVal
    }
  }

  override def returnFromInstanceInvoke(s: D, invokeExpr: InvokeExpr): Option[D] = {
    None
  }


  override def returnFromInvoke(invokeExpr: InvokeExpr): Option[D] = {
    None
  }

  override def newActivity(sootClass: SootClass): Option[D] = {
    Some(AbsValSet(Set(RecorderAbsValue.Activity(sootClass))))
  }
}
