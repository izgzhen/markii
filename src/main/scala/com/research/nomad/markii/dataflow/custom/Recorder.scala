/*
 * Copyright (c) 2020. Zhen Zhang
 */

package com.research.nomad.markii.dataflow.custom

import com.research.nomad.markii.dataflow.{AbsValSet, CustomObjectStateTransformer, Ref}
import soot.SootClass
import soot.jimple.{InstanceInvokeExpr, InvokeExpr}

sealed abstract class RecorderAbsValue extends Product with Serializable

object RecorderAbsValue {
  final case class MediaRecorder(isRecording: Boolean) extends RecorderAbsValue
  final case class Activity(sootClass: SootClass) extends RecorderAbsValue
}


object Recorder extends CustomObjectStateTransformer[AbsValSet[RecorderAbsValue]] {
  type D = AbsValSet[RecorderAbsValue]
  override def initInstance(sootClass: SootClass): Option[D] = {
    if (sootClass.getName == "android.media.MediaRecorder") {
      Some(AbsValSet(Set(RecorderAbsValue.MediaRecorder(isRecording = false))))
    } else {
      None
    }
  }

  override def updatedInstance(prevVal: D, instanceInvokeExpr: InstanceInvokeExpr): D = {
    if (instanceInvokeExpr.getMethod.getSignature == "<android.media.MediaRecorder: void start()>") {
      AbsValSet(Set(RecorderAbsValue.MediaRecorder(isRecording = true)))
    } else if (instanceInvokeExpr.getMethod.getSignature == "<android.media.MediaRecorder: void stop()>") {
      AbsValSet(Set(RecorderAbsValue.MediaRecorder(isRecording = false)))
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
