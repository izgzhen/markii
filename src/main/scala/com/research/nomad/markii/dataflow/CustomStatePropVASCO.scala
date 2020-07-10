/*
 * Copyright (c) 2020. Zhen Zhang
 */

package com.research.nomad.markii.dataflow

import com.research.nomad.markii.analyses.PreVASCO
import soot.{Local, SootMethod, Value}
import soot.jimple.{AssignStmt, CastExpr, InstanceFieldRef, InstanceInvokeExpr}
import soot.jimple.{NewExpr, ReturnStmt, StaticFieldRef, Stmt}
import soot.jimple.internal.JimpleLocal
import vasco.{Context, ForwardInterProceduralAnalysis, ProgramRepresentation}

/**
 * TODO: implement AbstractValuePropVASCO with it
 * @param entryPoints entry-point methods
 * @tparam V Object abstract state
 */
class CustomStatePropVASCO[V](entryPoints: List[SootMethod])
  extends ForwardInterProceduralAnalysis[SootMethod, soot.Unit, CustomDomain[V]] {
  type Domain = CustomDomain[V]
  type DomainContext = Context[SootMethod, soot.Unit, Domain]

  private val RETURN_LOCAL = new JimpleLocal("@return", null)

  def assigned(ctxMethod: SootMethod, assignStmt: Stmt, ref: Ref,
               value: Value, ctx: Domain, input: Domain): Domain = {
    val killed = input.killRef(ref)
    value match {
      case castExpr: CastExpr =>
        assigned(ctxMethod, assignStmt, ref, castExpr.getOp, ctx, input)
      case newExpr: NewExpr =>
        val sootClass = newExpr.getBaseType.getSootClass
        killed.newInstance(ref, sootClass, assignStmt)
      case rhsLocal: Local =>
        ctx.getAccessPath(rhsLocal) match {
          case Some(accessPath) =>
            killed.withSubPath(ref, accessPath)
          case None => killed
        }
      case instanceFieldRef: InstanceFieldRef =>
        val rhsLocalRef = Ref.from(instanceFieldRef)
        input.getAccessPath(rhsLocalRef) match {
          case Some(accessPath) =>
            killed.withSubPath(ref, accessPath)
          case None => killed
        }
      case staticFieldRef: StaticFieldRef =>
        val rhsLocalRef = Ref.from(staticFieldRef)
        input.getAccessPath(rhsLocalRef) match {
          case Some(accessPath) =>
            killed.withSubPath(ref, accessPath)
          case None => killed
        }
      case invokeExpr: InstanceInvokeExpr =>
        killed.invokeMethodAssign(ref, invokeExpr, assignStmt)
      case _ => killed
    }
  }

  override def normalFlowFunction(context: DomainContext, unit: soot.Unit, d: Domain): Domain = {
    unit match {
      case assignStmt: AssignStmt =>
        val ref = Ref.from(assignStmt.getLeftOp)
        return assigned(context.getMethod, assignStmt, ref, assignStmt.getRightOp, d, d)
      case retStmt: ReturnStmt =>
        return assigned(context.getMethod, retStmt, Ref.LocalRef(RETURN_LOCAL), retStmt.getOp, d, d)
      case _ =>
    }
    d
  }

  override def callEntryFlowFunction(context: DomainContext, m: SootMethod,
                                     callSite: soot.Unit, d: Domain): Domain = {
    val stmt = callSite.asInstanceOf[Stmt]
    val invokeExpr = stmt.getInvokeExpr
    var ret = if (PreVASCO.isStartWindowStmt(callSite.asInstanceOf[Stmt])) {
      topValue()
    } else {
      d.getHeap
    }
    if (m.hasActiveBody) {
      for (i <- 0 until invokeExpr.getArgCount) {
        ret = assigned(context.getMethod, stmt, Ref.LocalRef(m.getActiveBody.getParameterLocal(i)),
          invokeExpr.getArg(i), d, ret)
      }
      invokeExpr match {
        case instanceInvokeExpr: InstanceInvokeExpr =>
          ret = assigned(context.getMethod, stmt, Ref.LocalRef(m.getActiveBody.getThisLocal),
            instanceInvokeExpr.getBase, d, ret)
        case _ =>
      }
    }
    ret
  }

  // ???: How is the exit flow joined with the flow in the caller context? How is the caller context updated?
  override def callExitFlowFunction(context: DomainContext, m: SootMethod, unit: soot.Unit, d: Domain): Domain = {
    var ret = d.getHeap
    unit match {
      case assignStmt: AssignStmt =>
        ret = assigned(context.getMethod, assignStmt, Ref.LocalRef(assignStmt.getLeftOp.asInstanceOf[Local]), RETURN_LOCAL, d, ret)
      case _ =>
    }
    unit.asInstanceOf[Stmt].getInvokeExpr match {
      case instanceInvokeExpr: InstanceInvokeExpr =>
        val base = instanceInvokeExpr.getBase.asInstanceOf[Local]
        if (instanceInvokeExpr.getMethod.hasActiveBody) {
          val thisLocal = instanceInvokeExpr.getMethod.getActiveBody.getThisLocal
          ret = assigned(context.getMethod, unit.asInstanceOf[Stmt], Ref.LocalRef(base), thisLocal, d, ret)
        }
      case _ =>
    }
    ret
  }

  override def callLocalFlowFunction(context: DomainContext, unit: soot.Unit, d: Domain): Domain = {
    unit match {
      case assignStmt: AssignStmt =>
        val ref = Ref.from(assignStmt.getLeftOp)
        assigned(context.getMethod, assignStmt, ref, assignStmt.getRightOp, d, d)
      case _ =>
        val stmt = unit.asInstanceOf[Stmt]
        if (stmt.containsInvokeExpr()) {
          d.invokeMethod(stmt.getInvokeExpr, stmt)
        }
        d
    }
  }

  override def boundaryValue(m: SootMethod): Domain = topValue()

  override def copy(d: Domain): Domain = d

  private val aftProgramRepresentation = new AFTProgramRepresentation(entryPoints)
  override def programRepresentation(): ProgramRepresentation[SootMethod, soot.Unit] =
    aftProgramRepresentation

  override def meet(d1: Domain, d2: Domain): Domain = d1.meet(d2)

  private val top: Domain = CustomDomain.makeTop[V]
  override def topValue(): Domain = top
}
