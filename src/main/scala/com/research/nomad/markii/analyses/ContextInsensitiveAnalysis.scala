package com.research.nomad.markii.analyses

import java.util

import com.research.nomad.markii.Core
import com.research.nomad.markii.dataflow.{AFTDomain, AbstractValuePropVASCO}
import soot.SootMethod
import soot.jimple.toolkits.ide.icfg.BiDiInterproceduralCFG
import vasco.{DataFlowSolution, ProgramRepresentation}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

case class AnalyzedMethod[A](var entryValue: A, var exitValue: Option[A] = None) { }


case class ContextInsensitiveAnalysisMixin(core: Core, preVasco: PreVASCO, entryPoints: List[SootMethod],
                                           analysis: ContextInsensitiveAnalysis)
  extends AbstractValuePropVASCO(core, preVasco, entryPoints) {

  override def onNewCall(method: SootMethod, invocation: soot.Unit): Unit = this.onNewCall(method, invocation)
}

case class WorkListItem[M, N](method: M, unit: N)

abstract class ContextInsensitiveAnalysisImpl[M, N, A](icfg: BiDiInterproceduralCFG[N, M]) extends InterProcDataAnalysis[M, N, A]{
  protected val workList: mutable.Queue[WorkListItem[M, N]] = mutable.Queue()
  private val outValues: mutable.Map[N, A] = mutable.Map()
  private val inValues: mutable.Map[N, A] = mutable.Map()
  private val initializedMethods: mutable.Map[M, AnalyzedMethod[A]] = mutable.Map()

  def onNewCall(method: M, invocation: N): Unit = {
    setValueBefore(invocation, topValue)
    setValueAfter(invocation, topValue)
    workList.enqueue(WorkListItem(method, invocation))
  }

  def getMeetOverValidPathsSolution: DataFlowSolution[N, A] = {
    new DataFlowSolution[N, A](inValues.asJava, outValues.asJava)
  }

  def getMethods: Iterable[M] = initializedMethods.keys

  def doAnalysis(): Unit = {
    for (method <- programRepresentation.getEntryPoints.asScala) {
      initMethodEntryValue(method, boundaryValue(method))
    }

    while (!workList.isEmpty) {
      val item = workList.dequeue()
      val node = item.unit
      if (node != null) {
        val predecessors = icfg.getPredsOf(node)
        if (predecessors.size != 0) { // Initialise to the TOP value
          var in = topValue
          // Merge OUT values of all predecessors
          for (pred <- predecessors.asScala) {
            val predOut = outValues(pred)
            in = meet(in, predOut)
          }
          // Set the IN value at the node to the result
          inValues.put(node, in)
        }

        // Store the value of OUT before the flow function is processed.
        val prevOut = outValues(node)

        // Get the value of IN
        val in = inValues(node)

        // Now to compute the OUT value
        var out: Option[A] = None

        val contextMethod = item.method

        // Handle flow functions depending on whether this is a call statement or not
        if (programRepresentation.isCall(node)) {
          out = Some(topValue)
          var hit = false
          if (!programRepresentation.resolveTargets(contextMethod, node).isEmpty) {
            for (targetMethod <- programRepresentation.resolveTargets(contextMethod, node).asScala) {
              val entryValue = callEntryFlowFunction(targetMethod, targetMethod, node, in)

              var m = initializedMethods.get(targetMethod) match {
                case Some(m) => m
                case None => initMethodEntryValue(targetMethod, entryValue)
              }

              // Check if the target context has been analysed (surely not if it is just newly made):
              if (m.exitValue.isDefined) {
                hit = true
                val exitValue = m.exitValue.get
                val returnedValue = callExitFlowFunction(contextMethod, targetMethod, node, exitValue)
                out = Some(meet(out.get, returnedValue))
              }
            }
            // If there was at least one hit, continue propagation
            if (hit) {
              val localValue = callLocalFlowFunction(contextMethod, node, in)
              out = Some(meet(out.get, localValue))
            }
            else out = Some(callLocalFlowFunction(contextMethod, node, in))
          }
          else { // handle phantom method
            out = Some(callLocalFlowFunction(contextMethod, node, in))
          }
        }  else {
          out = Some(normalFlowFunction(contextMethod, node, in))
        }

        // Merge with previous OUT to force monotonicity (harmless if flow functions are monotinic)
        out = Some(meet(out.get, prevOut))

        // Set the OUT value
        outValues.put(node, out.get)

        // If OUT has changed...
        if (!(out.get == prevOut)) { // Then add successors to the work-list.
          for (successor <- icfg.getSuccsOf(node).asScala) {
            workList.enqueue(WorkListItem(contextMethod, successor))
          }
        }
      }
    }
  }

  def setValueAfter(node: N, value: A): Unit = {
    outValues.put(node, value)
  }

  def setValueBefore(node: N, value: A): Unit = {
    inValues.put(node, value)
  }

  def initMethodEntryValue(method: M, entryValue: A): AnalyzedMethod[A]
}
case class ContextInsensitiveAnalysis(core: Core, preVasco: PreVASCO, entryPoints: List[SootMethod])
  extends ContextInsensitiveAnalysisImpl[SootMethod, soot.Unit, AFTDomain](core.icfg) {
  val mixin = ContextInsensitiveAnalysisMixin(core, preVasco, entryPoints, this)

  override def initMethodEntryValue(method: SootMethod, entryValue: AFTDomain): AnalyzedMethod[AFTDomain] = {
    val analyzedMethod = AnalyzedMethod(copy(entryValue), Some(topValue))
    for (unit <- method.getActiveBody.getUnits.asScala) {
      setValueAfter(unit, topValue)
      setValueBefore(unit, topValue)
      workList.enqueue(WorkListItem(method, unit))
    }

    for (unit <- core.icfg.getStartPointsOf(method).asScala) {
      setValueBefore(unit, copy(entryValue))
    }

    analyzedMethod
  }

  override def normalFlowFunction(contextMethod: SootMethod, node: soot.Unit, inValue: AFTDomain): AFTDomain =
    mixin.normalFlowFunction(contextMethod, node, inValue)

  override def callEntryFlowFunction(contextMethod: SootMethod, targetMethod: SootMethod, node: soot.Unit, inValue: AFTDomain): AFTDomain =
    mixin.callEntryFlowFunction(contextMethod, targetMethod, node, inValue)

  override def callExitFlowFunction(contextMethod: SootMethod, targetMethod: SootMethod, node: soot.Unit, exitValue: AFTDomain): AFTDomain =
    mixin.callExitFlowFunction(contextMethod, targetMethod, node, exitValue)

  override def callLocalFlowFunction(contextMethod: SootMethod, node: soot.Unit, inValue: AFTDomain): AFTDomain =
    mixin.callLocalFlowFunction(contextMethod, node, inValue)

  override def programRepresentation: ProgramRepresentation[SootMethod, soot.Unit] =
    mixin.programRepresentation()

  override def topValue: AFTDomain = mixin.topValue()

  override def meet(op1: AFTDomain, op2: AFTDomain): AFTDomain = mixin.meet(op1, op2)

  override def boundaryValue(entryPoint: SootMethod): AFTDomain = mixin.boundaryValue(entryPoint)

  override def copy(src: AFTDomain): AFTDomain = mixin.copy(src)
}