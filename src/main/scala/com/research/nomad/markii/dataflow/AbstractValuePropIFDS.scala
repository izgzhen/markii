/*
 * Copyright (c) 2020. Zhen Zhang
 */

package com.research.nomad.markii.dataflow

import java.{lang, util}
import java.util.concurrent.ConcurrentHashMap

import com.research.nomad.markii.Core
import heros.flowfunc.{Identity, KillAll}
import heros.{DefaultSeeds, FlowFunction, FlowFunctions, InterproceduralCFG}
import soot.jimple.internal.JimpleLocal
import soot.jimple.toolkits.ide.DefaultJimpleIFDSTabulationProblem
import soot.jimple.{ClassConstant, DefinitionStmt, InstanceInvokeExpr, Jimple, NewExpr, ReturnStmt, Stmt, StringConstant}
import soot.{EquivalentValue, NullType, RefType, Scene, SootMethod, Value}

import scala.jdk.CollectionConverters._

/**
 * Implementation of abstract value propagation for IFDS DFA framework. Currently only implements:
 * - Simple intent analysis
 */
class AbstractValuePropIFDS(core: Core)
  extends DefaultJimpleIFDSTabulationProblem[(Value, Set[AbstractValue]), InterproceduralCFG[soot.Unit, SootMethod]](core.icfg) {
  type Domain = (Value, Set[AbstractValue])
  private val id = Identity.v[Domain]()
  private val killAll = KillAll.v[Domain]()
  private val zero = zeroValue()
  val visitedMethods: ConcurrentHashMap.KeySetView[SootMethod, lang.Boolean] = ConcurrentHashMap.newKeySet[SootMethod](100)

  private def putUnitAbstractions(u: soot.Unit): Unit = {
    val method = interproceduralCFG.getMethodOf(u)
    if (core.isDebugMode) println(s"putUnitAbstractions: $method")
    visitedMethods.add(method)
  }

  override def createFlowFunctionsFactory(): FlowFunctions[soot.Unit, Domain, SootMethod] =
    new FlowFunctions[soot.Unit, Domain, SootMethod]() {
      override def getNormalFlowFunction(curr: soot.Unit, succ: soot.Unit): FlowFunction[Domain] = {
        curr match {
          case defStmt: DefinitionStmt =>
            source: Domain => {
              val s: Set[Domain] = if (source != zero) {
                putUnitAbstractions(curr)
                val (tainted, _) = source
                if (tainted.equivTo(defStmt.getLeftOp)) {
                  Set()
                } else {
                  Set(source)
                }
              } else {
                defStmt.getRightOp match {
                  case newExpr: NewExpr =>
                    if (newExpr.getType.toString == "android.content.Intent") {
                      Set((defStmt.getLeftOp, Set(AbstractValue.Intent(new AbstractIntent()))))
                    } else {
                      Set()
                    }
                  case _ => Set()
                }
              }
              s.asJava
            }
          case _ => id
        }
      }

      override def getCallFlowFunction(callStmt: soot.Unit, destinationMethod: SootMethod): FlowFunction[Domain] = {
        val stmt = callStmt.asInstanceOf[Stmt]
        val invokeExpr = stmt.getInvokeExpr
        val args = invokeExpr.getArgs
        source: Domain => {
          val s: Set[Domain] =
            if (destinationMethod.getName == "<clinit>" ||
              destinationMethod.getSubSignature == "void run()") {
            Set(source)
          } else if (source != zero) {
            putUnitAbstractions(callStmt)
            val (tainted, taints) = source
            if (args.contains(tainted)) {
              val paramIndex = args.indexOf(tainted)
              val param: Value = new EquivalentValue(
                Jimple.v.newParameterRef(destinationMethod.getParameterType(paramIndex), paramIndex))
              Set((param, taints))
            } else {
              Set()
            }
          } else {
            Set()
          }
          s.asJava
        }
      }

      override def getReturnFlowFunction(callSite: soot.Unit, calleeMethod: SootMethod,
                                         exitStmt: soot.Unit, returnSite: soot.Unit): FlowFunction[Domain] = {
        callSite match {
          case defStmt:DefinitionStmt =>
            exitStmt match {
              case retStmt:ReturnStmt =>
                source: Domain => {
                  val s: Set[Domain] = if (source != zero) {
                    putUnitAbstractions(exitStmt)
                    val (tainted, taints) = source
                    if (retStmt.getOp.equivTo(tainted)) {
                      Set((defStmt.getLeftOp, taints))
                    } else {
                      Set()
                    }
                  } else {
                    Set()
                  }
                  s.asJava
                }
              case _ => killAll
            }
          case _ => killAll
        }
      }

      def getStrConstants(value: Value): Set[String] = {
        value match {
          case strConst: StringConstant => Set(strConst.value)
          case _ => Set()
        }
      }

      override def getCallToReturnFlowFunction(callSite: soot.Unit, returnSite: soot.Unit): FlowFunction[Domain] = {
        val stmt = callSite.asInstanceOf[Stmt]
        val invokeExpr = stmt.getInvokeExpr
        source: Domain => {
          val s: Set[Domain] = if (source != zero) {
            val (tainted, taints) = source
            invokeExpr match {
              case instanceInvokeExpr: InstanceInvokeExpr =>
                instanceInvokeExpr.getMethod.getSignature match {
                  case "<android.content.Intent: void <init>(android.content.Context,java.lang.Class)>" =>
                    instanceInvokeExpr.getArg(1) match {
                      case classConst: ClassConstant =>
                        Set((tainted, taints.flatMap {
                          case AbstractValue.Intent(i) =>
                            Some(AbstractValue.Intent(i.withTargets(Set(classConst.toSootType.asInstanceOf[RefType].getSootClass))))
                          case _ => None
                        }))
                      case _ => Set(source)
                    }
                  case "<android.content.Intent: android.content.Intent setAction(java.lang.String)>" =>
                    val strings = getStrConstants(instanceInvokeExpr.getArg(0))
                    Set((tainted, taints.flatMap {
                      case AbstractValue.Intent(i) => Some(AbstractValue.Intent(i.withActions(strings)))
                      case _ => None
                    }))
                  case _ => Set(source)
                }
              case _ => Set(source)
            }
          } else {
            Set()
          }
          s.asJava
        }
      }
    }

  // FIXME: entrypoints might not be enough
  override def initialSeeds(): util.Map[soot.Unit, util.Set[(Value, Set[AbstractValue])]] = {
    DefaultSeeds.make(Scene.v.getEntryPoints.asScala.filter(_.hasActiveBody).map(_.getActiveBody.getUnits.getFirst).asJava, zeroValue())
  }

  override def createZeroValue: (Value, Set[AbstractValue]) = (new JimpleLocal("<<zero>>", NullType.v), Set())
}
