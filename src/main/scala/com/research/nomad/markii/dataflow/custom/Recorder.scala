/*
 * Copyright (c) 2020. Zhen Zhang
 */

package com.research.nomad.markii.dataflow.custom

import com.research.nomad.markii.dataflow.CustomObjectStateTransformer
import soot.SootClass
import soot.jimple.InstanceInvokeExpr

object Recorder extends CustomObjectStateTransformer[Boolean] {
  override def initInstance(sootClass: SootClass): Option[Boolean] = {
    if (sootClass.getName == "android.media.MediaRecorder") {
      Some(false)
    } else {
      None
    }
  }

  override def updatedInstance(isRecording: Boolean, instanceInvokeExpr: InstanceInvokeExpr): Boolean = {
    if (instanceInvokeExpr.getMethod.getSignature == "<android.media.MediaRecorder: void start()>") {
      true
    } else {
      isRecording
    }
  }
}
