/*
 * Copyright (c) 2020. Zhen Zhang
 */

package com.research.nomad.markii

import java.io.PrintWriter

import com.research.nomad.markii.dataflow.AbsNode.ViewNode
import com.research.nomad.markii.dataflow.{AFTDomain, AbstractValue, AbstractValuePropIFDS, AbstractValuePropVASCO}
import com.research.nomad.markii.instrument.{DialogCreateInstrument, DialogInitInstrument}
import heros.InterproceduralCFG
import heros.solver.IFDSSolver
import io.github.izgzhen.msbase.IOUtil
import presto.android.gui.IDNameExtractor
import presto.android.gui.wtg.util.WTGUtil
import presto.android.{Configs, Debug, Hierarchy, MethodNames}
import presto.android.xml.{AndroidView, XMLParser}
import soot.jimple.toolkits.callgraph.Edge
import soot.jimple.{EqExpr, IfStmt, InstanceInvokeExpr, IntConstant, Jimple, JimpleBody, LookupSwitchStmt, Stmt, StringConstant}
import soot.jimple.toolkits.ide.icfg.JimpleBasedInterproceduralCFG
import soot.jimple.toolkits.pointer.LocalMustAliasAnalysis
import soot.toolkits.graph.CompleteUnitGraph
import soot.toolkits.scalar.{SimpleLiveLocals, SmartLocalDefs}
import soot.{Body, Local, RefType, Scene, SootClass, SootMethod, Value}
import vasco.{DataFlowSolution, Helper}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

case class Runner(method: SootMethod, loopExit: soot.Unit, view: Local)

/**
 * Core GUI Analysis that implement the client interface IAnalysis
 */
object GUIAnalysis extends IAnalysis {
  val hier: Hierarchy = Hierarchy.v()
  val xmlParser: XMLParser = XMLParser.Factory.getXMLParser
  private var writer: FactsWriter = _

  def getIdName(id: Int): Option[String] = {
    val idName = IDNameExtractor.v.idName(id)
    if (idName != null && idName.length > 0) {
      Some(idName)
    } else {
      None
    }
  }

  def getIdName(node: ViewNode): Set[String] = {
    node.id.flatMap(i => getIdName(i))
  }

  def analyzeViewNode(viewNode: ViewNode, ownerActivities: Set[SootClass]): Unit = {
    viewNode.id.foreach(id => {
      getIdName(id) match {
        case Some(idName) =>
          writer.writeConstraint(FactsWriter.Fact.idName, idName, viewNode.nodeID)
        case None =>
      }
    })
    // TODO: analyze the mutations of default values
    var hasContentDescription = false
    for ((attrib, value) <- viewNode.getAttrs) {
      if (value != null) {
        attrib match {
          case AndroidView.ViewAttr.layout_height =>
            writer.writeDimensionConstraint(FactsWriter.Fact.layoutHeight, value, viewNode.nodeID)
          case AndroidView.ViewAttr.layout_width =>
            writer.writeDimensionConstraint(FactsWriter.Fact.layoutWidth, value, viewNode.nodeID)
          case AndroidView.ViewAttr.textSize =>
            writer.writeDimensionConstraint(FactsWriter.Fact.textSize, value, viewNode.nodeID)
          case AndroidView.ViewAttr.background =>
            writer.writeConstraint(FactsWriter.Fact.background, value, viewNode.nodeID)
          case AndroidView.ViewAttr.text =>
            writer.writeConstraint(FactsWriter.Fact.textContent, value, viewNode.nodeID)
            if (value.toLowerCase().contains("rec")) {
              writer.writeConstraint(FactsWriter.Fact.recordButton, viewNode.nodeID)
            }
            if (value.toLowerCase().contains("continue")) {
              writer.writeConstraint(FactsWriter.Fact.actionButton, viewNode.nodeID)
            }
          case AndroidView.ViewAttr.contentDescription => hasContentDescription = true
          case _ =>
        }
      }
    }
    for ((eventType, methodName) <- viewNode.getInlineClickHandlers) {
      for (act <- ownerActivities) {
        val method = act.getMethodByNameUnsafe(methodName)
        if (method != null) {
          writer.writeConstraint(FactsWriter.Fact.eventHandler, eventType, method, viewNode.nodeID)
          analyzeAnyHandlerPostVASCO(method)
        }
      }
    }
    for ((attrib, value) <- viewNode.getAppAttrs) {
      if (value != null) {
        writer.writeConstraint(FactsWriter.Fact.withName(attrib.name()), viewNode.nodeID, value)
      }
    }
    viewNode.sootClass match {
      case Some(c) =>
        if (!hasContentDescription && hier.isSubclassOf(c, Constants.imageViewClass)) {
          writer.writeConstraint(FactsWriter.Fact.imageHasNoContentDescription, viewNode.nodeID)
        }
        if (Constants.isAdViewClass(c)) {
          writer.writeConstraint(FactsWriter.Fact.adViewClass, c)
        }
        writer.writeConstraint(FactsWriter.Fact.viewClass, c.getName, viewNode.nodeID)
        if (hier.isSubclassOf(c, Constants.buttonViewClass)) writer.writeConstraint(FactsWriter.Fact.buttonView, viewNode.nodeID)
        if (Constants.isDialogClass(c)) writer.writeConstraint(FactsWriter.Fact.dialogView, viewNode.nodeID, icfg.getMethodOf(viewNode.allocSite))
      case None =>
    }
  }

  def analyzeActivityHandlerPostVasco(handler: SootMethod): Unit = {
    for (endpoint <- icfg.getEndPointsOf(handler).asScala) {
      val aftDomain = vascoSolution.getValueAfter(endpoint)
      if (aftDomain != null) {
        // TODO: inspect view nodes at the end of each activity handler
        // NOTE: there is extra computation, but do we care?
        for ((node, children) <- aftDomain.nodeEdgeMap) {
          val ownerActivities = aftDomain.getOwnerActivities(handler, endpoint.asInstanceOf[Stmt], node)
          analyzeViewNode(node, ownerActivities)
          for (child <- children) {
            writer.writeConstraint(FactsWriter.Fact.containsView, node.nodeID, child.nodeID)
            analyzeViewNode(child, ownerActivities)
          }
        }
      }
    }
  }

  private def dumpCallgraph(): Unit = {
    val printWriter: PrintWriter = new PrintWriter("/tmp/icfg.txt")
    printWriter.print(Scene.v().getCallGraph.toString.replace(" ==> ", "\n\t==> "))
    printWriter.close()
  }

  private val aliasAnalysisMap = mutable.Map[SootMethod, LocalMustAliasAnalysis]()
  private val localDefsMap = mutable.Map[SootMethod, SmartLocalDefs]()

  def getLocalDefs(m: SootMethod) : SmartLocalDefs = {
    if (!localDefsMap.contains(m)) {
      val ug = new CompleteUnitGraph(m.getActiveBody)
      localDefsMap.addOne(m, new SmartLocalDefs(ug, new SimpleLiveLocals(ug)))
    }
    localDefsMap(m)
  }

  def getDefsOfAt(m: SootMethod, l: Local, u: soot.Unit): Set[Stmt] = {
    getLocalDefs(m).getDefsOfAt(l, u).asScala.toSet.map((u: soot.Unit) => u.asInstanceOf[Stmt])
  }

  def isAlias(l1: Local, l2: Local, stmt1: Stmt, stmt2: Stmt, m: SootMethod): Boolean = {
    if (l1.equivTo(l2)) {
      return true
    }
    if (!aliasAnalysisMap.contains(m)) {
      val ug = new CompleteUnitGraph(m.getActiveBody)
      aliasAnalysisMap.addOne(m, new LocalMustAliasAnalysis(ug, false))
    }
    val analysis = aliasAnalysisMap(m)
    analysis.mustAlias(l1, stmt1, l2, stmt2) || getLocalDefs(m).getDefsOf(l2) == getLocalDefs(m).getDefsOf(l1)
  }

  private var allHandlers: Set[SootMethod] = _
  private val analyzedMethods = mutable.Set[SootMethod]()

  def initAllHandlers(): Set[SootMethod] = {
    val handlers = mutable.Set[SootMethod]()
    for (c <- Scene.v().getApplicationClasses.asScala) {
      if (c.isConcrete && Constants.guiClasses.exists(listener => hier.isSubclassOf(c, listener))) {
        for (m <- c.getMethods.asScala) {
          if (m.hasActiveBody && m.getName.startsWith("on")) {
            handlers.add(m)
          }
        }
      }
    }

    for (receiver <- xmlParser.getReceivers.asScala) {
      if (receiver != null) {
        val curCls = Scene.v.getSootClassUnsafe(receiver)
        if (curCls != null && curCls.isConcrete && hier.isSubclassOf(curCls, Scene.v().getSootClass("android.content.BroadcastReceiver"))) {
          val m = curCls.getMethodByNameUnsafe("onReceive")
          if (m != null && m.hasActiveBody) {
            handlers.add(m)
          }
        }
      }
    }
    handlers.toSet
  }

  private var vascoSolution: DataFlowSolution[soot.Unit, AFTDomain] = _
  def runVASCO(): Unit = {
    // NOTE: over-approx of entrypoints
    val entrypointsFull = allActivities.flatMap(DynamicCFG.getRunner).map(_.method).toList
    val vascoProp = new AbstractValuePropVASCO(entrypointsFull)
    println("VASCO starts")
    vascoProp.doAnalysis()
    println("VASCO finishes")

    analyzedMethods.addAll(allHandlers)
    analyzedMethods.addAll(vascoProp.getMethods.asScala)
    if (sys.env.contains("BATCH_RUN")) {
      vascoSolution = Helper.getMeetOverValidPathsSolution(vascoProp)
    } else {
      vascoSolution = Helper.getMeetOverValidPathsSolutionPar(vascoProp)
    }
    println("VASCO solution generated")
  }

  var outputPath = "/tmp/markii-facts/"
  var debugMode = false

  def readConfigs(): Unit = {
    for (param <- Configs.clientParams.asScala) {
      if (param.startsWith("output:")) outputPath = param.substring("output:".length)
    }

    debugMode = Configs.clientParams.contains("debugMode:true")
  }

  private var ifdsSolver: IFDSSolver[soot.Unit, (Value, Set[AbstractValue]), SootMethod, InterproceduralCFG[soot.Unit, SootMethod]] = _
  private var icfg: JimpleBasedInterproceduralCFG = _

  def getIfdsResultAt(stmt: Stmt, target: Value): Iterable[AbstractValue] = {
    ifdsSolver.ifdsResultsAt(stmt).asScala.flatMap { case (tainted, taints) =>
      if (tainted.equivTo(target)) {
        taints
      } else {
        Set()
      }
    }
  }

  def runIFDS(): Unit = {
    icfg = new JimpleBasedInterproceduralCFG()
    val analysis = new AbstractValuePropIFDS(icfg)
    ifdsSolver = new IFDSSolver(analysis)
    System.out.println("======================== IFDS Solver started  ========================")
    val entrypoints = Scene.v().getEntryPoints.asScala.filter(m => {
      val c = m.getDeclaringClass
      Constants.isActivity(c) || Constants.isService(c) || Constants.isReceiver(c)
    })
    Scene.v().setEntryPoints(entrypoints.asJava)
    System.out.println("======================= IFDS Entry-points " + Scene.v().getEntryPoints.size() + " =======================")
    IOUtil.writeLines(Scene.v().getEntryPoints.asScala.map(_.toString).toList, "/tmp/entrypoints.txt")
    // TODO: Develop a method to debug performance issue here
    ifdsSolver.solve()
    analyzedMethods.addAll(analysis.visitedMethods)
    System.out.println("======================== IFDS Solver finished ========================")
  }

  private val allActivities = hier.frameworkManaged.keySet().asScala.filter(c => hier.applicationActivityClasses.contains(c) && !c.isAbstract).toSet

  def writeConstraintsPostVASCO(): Unit = {
    for (act <- allActivities) {
      for ((handler, event) <- getActivityHandlers(act)) {
        analyzeAnyHandlerPostVASCO(handler)
        writer.writeConstraint(FactsWriter.Fact.activityEventHandler, event, handler, act)
        analyzeActivityHandlerPostVasco(handler)
      }
      val lifecycleMethods = List(
        act.getMethodUnsafe(MethodNames.onActivityCreateSubSig),
        act.getMethodUnsafe(MethodNames.onActivityStartSubSig)
      )
      for (m <- lifecycleMethods) {
        if (m != null) {
          analyzeAnyHandlerPostVASCO(m)
          analyzeActivityHandlerPostVasco(m)
        }
      }
    }

    for (handler <- allHandlers) {
      analyzeAnyHandlerPostVASCO(handler)
    }

    if (mainActivity != null) {
      writer.writeConstraint(FactsWriter.Fact.mainActivity, mainActivity)
    }
    val newlpMainActivity = Scene.v.getSootClassUnsafe("com.revlwp.wallpaper.newlp.MainActivity")
    if (newlpMainActivity != null) {
      writer.writeConstraint(FactsWriter.Fact.mainActivity, newlpMainActivity)
    } 

    for ((className, filters) <- xmlParser.getIntentFilters.asScala) {
      try {
        val curCls = Scene.v.getSootClass(className)
        if (curCls != null && curCls.isConcrete) {
          for (filter <- filters.asScala) {
            for (action <- filter.getActions.asScala) {
              val m = curCls.getMethodByNameUnsafe("onReceive")
              if (m != null) writer.writeConstraint(FactsWriter.Fact.intentFilter, action, m)
            }
          }
        }
      } catch {
        case ignored: NullPointerException =>
      }
    }

    writer.close()
  }

  private val prefActivity = Scene.v.getSootClass("android.preference.PreferenceActivity")

  def analyzeActivityPreVasco(activityClass: SootClass): Unit = {
    for ((handler, _) <- getActivityHandlers(activityClass)) {
      DynamicCFG.addActivityHandlerToEventLoop(activityClass, handler)
    }
    val onCreate = hier.virtualDispatch(MethodNames.onActivityCreateSubSig, activityClass)
    if (onCreate != null) {
      writer.writeConstraint(FactsWriter.Fact.lifecycleMethod, activityClass, "onCreate", onCreate)
    }

    val onDestroy = hier.virtualDispatch(MethodNames.onActivityDestroySubSig, activityClass)
    if (onDestroy != null) {
      writer.writeConstraint(FactsWriter.Fact.lifecycleMethod, activityClass, "onDestroy", onDestroy)
    }
    if (hier.isSubclassOf(activityClass, prefActivity)) {
      writer.writeConstraint(FactsWriter.Fact.preferenceActivity, activityClass)
    }
  }

  private val showDialogInvocations = mutable.Map[Stmt, Local]()

  def instrumentRunOnUiThread(m: SootMethod): Unit = {
    val swaps = mutable.Map[Stmt, Stmt]()
    for (unit <- m.getActiveBody.getUnits.asScala) {
      val stmt = unit.asInstanceOf[Stmt]
      if (stmt.containsInvokeExpr()) {
        val invokeExpr = stmt.getInvokeExpr
        if (Constants.isActivityRunOnUiThread(invokeExpr.getMethod)) {
          val runnableArg = invokeExpr.getArg(0).asInstanceOf[Local]
          val runnableArgClass = runnableArg.getType.asInstanceOf[RefType].getSootClass
          val runMethod = runnableArgClass.getMethodByNameUnsafe("run")
          val invocation = Jimple.v().newInvokeStmt(Jimple.v().newVirtualInvokeExpr(runnableArg, runMethod.makeRef()))
          CallGraphManager.updateCall(m, stmt, invocation, runMethod)
          swaps.put(stmt, invocation)
        }
      }
    }
    for ((out, in) <- swaps) {
      m.getActiveBody.getUnits.swapWith(out, in)
    }
  }

  /**
   * Example: a8e458e619d5235fe8a97ea0186961acc1192ed13bb28a79b493c752747cc683
   * @param m
   */
  def instrumentStartTimer(m: SootMethod): Unit = {
    val swaps = mutable.Map[Stmt, Stmt]()
    for (unit <- m.getActiveBody.getUnits.asScala) {
      val stmt = unit.asInstanceOf[Stmt]
      if (stmt.containsInvokeExpr()) {
        val invokeExpr = stmt.getInvokeExpr
        if (invokeExpr.getMethod.getSignature == "<android.os.CountDownTimer: android.os.CountDownTimer start()>") {
          val timer = invokeExpr.asInstanceOf[InstanceInvokeExpr].getBase.asInstanceOf[Local]
          val timerClass = timer.getType.asInstanceOf[RefType].getSootClass
          val onFinish = timerClass.getMethodByNameUnsafe("onFinish")
          val invocation = Jimple.v().newInvokeStmt(Jimple.v().newVirtualInvokeExpr(timer, onFinish.makeRef()))
          CallGraphManager.updateCall(m, stmt, invocation, onFinish)
          swaps.put(stmt, invocation)
        }
      }
    }
    for ((out, in) <- swaps) {
      m.getActiveBody.getUnits.swapWith(out, in)
    }
  }

  def analyzeAnyHandlerPreVasco(handler: SootMethod): Unit = {
    val reachedMethods = CallGraphManager.reachableMethods(handler)
    for (reached <- reachedMethods) {
      if (reached.getDeclaringClass.isApplicationClass && reached.isConcrete && reached.hasActiveBody) {
        val swaps = mutable.Map[Stmt, Stmt]()
        for (unit <- reached.getActiveBody.getUnits.asScala) {
          val stmt = unit.asInstanceOf[Stmt]
          if (stmt.containsInvokeExpr()) {
            val invokeExpr = stmt.getInvokeExpr
            val invokedMethod = Util.getMethodUnsafe(invokeExpr)
            if (invokedMethod != null) {
              if (invokedMethod.getSubSignature == "void startActivity(android.content.Intent)") {
                // NOTE: the base type is only used to provide an application context, thus it can't be used to infer the
                //       source activity
                GUIAnalysis.getIfdsResultAt(stmt, invokeExpr.getArg(0)).foreach {
                  case AbstractValue.Intent(intent) =>
                    // FIXME: imprecision if we ignore the actions etc. fields?
                    val methods = mutable.Set[SootMethod]()
                    for (target <- intent.targets) {
                      DynamicCFG.getRunner(target) match {
                        case Some(runner) => methods.add(runner.method)
                        case None =>
                      }
                      if (Ic3Manager.ic3Enabled) {
                        writer.writeConstraint(FactsWriter.Fact.startActivity, handler, reached, target)
                      }
                    }
                    val base = invokeExpr.asInstanceOf[InstanceInvokeExpr].getBase.getType.asInstanceOf[RefType].getSootClass
                    val runAllMethod = DynamicCFG.getRunAll(stmt, methods, base, invokedMethod)
                    Scene.v().getCallGraph.removeAllEdgesOutOf(stmt)
                    Scene.v().getCallGraph.addEdge(new Edge(reached, stmt, runAllMethod))
                    // NOTE: no invocation/stmt swap
                    startWindowStmts.add(stmt)
                    invokeExpr.setMethodRef(runAllMethod.makeRef())
                  case _ =>
                }
              }

              // TODO: need to bind analyze listener setting in IFDS first
              // TODO: actually -- this is a more complicated recursive problem. but we can play it easy first.
              // FIXME: AlertDialog$Builder has a show method as well, this might not work well...but we take it as if it is a Dialog object now
              if (Constants.isDialogShow(invokedMethod.getSignature) ||
                Constants.isDialogBuilderShow(invokedMethod.getSignature)) {
                val dialogBase = invokeExpr.asInstanceOf[InstanceInvokeExpr].getBase.asInstanceOf[Local]
                val methods = mutable.Set[SootMethod]()
                for (defStmt <- getDefsOfAt(reached, dialogBase, stmt)) {
                  DynamicCFG.getRunnerOfDialog(defStmt) match {
                    case Some(runner) => methods.add(runner.method)
                    case None =>
                  }
                }
                if (methods.nonEmpty) {
                  // FIXME: I need to build a static class for this type of work
                  val runAllMethod = DynamicCFG.getRunAllDialog(stmt, methods, reached)
                  val invocation = Jimple.v().newInvokeStmt(Jimple.v().newStaticInvokeExpr(runAllMethod.makeRef(), dialogBase))
                  Scene.v().getCallGraph.removeAllEdgesOutOf(stmt)
                  Scene.v().getCallGraph.addEdge(new Edge(reached, invocation, runAllMethod))
                  swaps.put(stmt, invocation)
                  startWindowStmts.add(invocation)
                  showDialogInvocations.put(invocation, dialogBase)
                }
              }
              if (invokedMethod.getSubSignature == "void showDialog(int)") {
                invokeExpr.getArg(0) match {
                  case intConstant: IntConstant =>
                    DialogCreateInstrument.getShowInvocationOfCreateDialog(reached, intConstant.value) match {
                      case Some(createMethod) =>
                        DynamicCFG.getRunnerOfDialog(reached.getDeclaringClass, createMethod, intConstant) match {
                          case Some((runner, internalInvocation)) =>
                            val invocation = Jimple.v().newInvokeStmt(Jimple.v().newVirtualInvokeExpr(reached.getActiveBody.getThisLocal, runner.method.makeRef()))
                            CallGraphManager.updateCall(reached, stmt, invocation, runner.method, Some(handler))
                            swaps.put(stmt, invocation)
                            startWindowStmts.add(invocation)
                            showDialogInvocations.put(internalInvocation, runner.view)
                          case None =>
                        }
                      case _ =>
                    }
                  case _ =>
                }
              }
              // Replace loadNativeAds with the load handler
              if (invokedMethod.getName == "loadNativeAds") {
                for ((argType, idx) <- invokedMethod.getParameterTypes.asScala.zipWithIndex) {
                  if (argType.toString == "com.applovin.nativeAds.AppLovinNativeAdLoadListener") {
                    val listenerArg = invokeExpr.getArg(idx).asInstanceOf[Local]
                    val listenerClass = listenerArg.getType.asInstanceOf[RefType].getSootClass
                    val adLoadHandler = listenerClass.getMethodByNameUnsafe("onNativeAdsLoaded")
                    if (adLoadHandler != null && Constants.isActivity(reached.getDeclaringClass)) {
                      // Instrument adLoadHandler
                      instrumentRunOnUiThread(adLoadHandler)

                      // Replace invocation
                      val invocation = Jimple.v().newInvokeStmt(Jimple.v().newVirtualInvokeExpr(listenerArg, adLoadHandler.makeRef()))
                      CallGraphManager.updateCall(reached, stmt, invocation, adLoadHandler, Some(handler))
                      swaps.put(stmt, invocation)
                    }
                  }
                }
              }
            }
          }
        }

        for ((out, in) <- swaps) {
          reached.getActiveBody.getUnits.swapWith(out, in)
        }
      }
    }
  }

  private val analyzedHandlers = mutable.Set[SootMethod]()
  def analyzeAnyHandlerPostVASCO(handler: SootMethod): Unit = {
    if (analyzedHandlers.contains(handler)) {
      return
    }
    analyzedHandlers.add(handler)
    val reachedMethods = CallGraphManager.reachableMethods(handler)

    // Analyze the end-points
    for (endpoint <- icfg.getEndPointsOf(handler).asScala) {
      val aftDomain = vascoSolution.getValueAfter(endpoint)
      if (aftDomain != null) {
        for (((viewNode, eventType), eventHandlers) <- aftDomain.nodeHandlerMap) {
          for (eventHandler <- eventHandlers) {
            writer.writeConstraint(FactsWriter.Fact.eventHandler, eventType, eventHandler, viewNode.nodeID)
            analyzeAnyHandlerPostVASCO(eventHandler)
          }
        }
        for ((act, nodes) <- aftDomain.activityRootViewMap) {
          for (viewNode <- nodes) {
            writer.writeConstraint(FactsWriter.Fact.rootView, act, viewNode.nodeID)
          }
        }
      } else {
        // FIXME
        println("[WARN] Null endpoint: " + handler)
      }
    }

    // Analyze each statements in reached method
    // FIXME: performance
    for (reached <- reachedMethods) {
      if (reached.getDeclaringClass.isApplicationClass && reached.isConcrete && reached.hasActiveBody) {
        for ((srcClass, intents) <- Ic3Manager.getIntents(reached)) {
          for (intent <- intents) {
            if (intent.getComponentClass != null && intent.getComponentClass.nonEmpty) {
              val targetAct = Scene.v().getSootClass(intent.getComponentClass)
              writer.writeConstraint(FactsWriter.Fact.startActivity, handler, reached, targetAct)
            }
            if (intent.getAction != null && intent.getAction.equals("android.intent.action.VIEW")) {
              if (intent.getDataScheme != null && intent.getDataScheme.equals("market")) {
                writer.writeConstraint(FactsWriter.Fact.startViewActivityOfMarketHost, handler, reached);
              } else {
                writer.writeConstraint(FactsWriter.Fact.startViewActivityOfSomeHosts, handler, reached);
              }
            }
          }
        }

        for (unit <- reached.getActiveBody.getUnits.asScala) {
          val stmt = unit.asInstanceOf[Stmt]
          if (stmt.containsInvokeExpr()) {
            val invokeExpr = stmt.getInvokeExpr
            val invokedMethod = Util.getMethodUnsafe(invokeExpr)
            val aftDomain = vascoSolution.getValueBefore(stmt)
            if (WTGUtil.v.isActivityFinishCall(stmt)) {
              invokeExpr match {
                case instanceInvokeExpr: InstanceInvokeExpr =>
                  val actClass = instanceInvokeExpr.getBase.getType.asInstanceOf[RefType].getSootClass
                  // FIXME: reachability
                  writer.writeConstraint(FactsWriter.Fact.finishActivity, handler, reached, actClass)
                case _ =>
              }
            }
            if (invokedMethod != null) {
              if (Constants.isDialogDismiss(invokedMethod.getSignature)) {
                val dialog = invokeExpr.asInstanceOf[InstanceInvokeExpr].getBase.asInstanceOf[Local]
                if (aftDomain != null) {
                  // NOTE: We can combine the for-loops here
                  for (dialogNode <- aftDomain.getViewNodes(reached, stmt, dialog)) {
                    writer.writeConstraint(FactsWriter.Fact.dismiss, handler, dialogNode.nodeID)
                  }
                }
              }
              // FIXME: frauddroid-gba.apk "<android.app.Activity: void showDialog(int)>" and onPrepareDialog, onCreateDialog
              if (showDialogInvocations.contains(stmt)) {
                if (aftDomain != null) {
                  val dialogBase = showDialogInvocations(stmt)
                  for (dialogNode <- aftDomain.getViewNodes(reached, stmt, dialogBase)) {
                    writer.writeConstraint(FactsWriter.Fact.dialogView, dialogNode.nodeID, icfg.getMethodOf(dialogNode.allocSite))
                    writer.writeConstraint(FactsWriter.Fact.showDialog, handler, reached, dialogNode.nodeID)
                    for (handler <- aftDomain.getDialogButtonHandlers(dialogNode)) {
                      writer.writeConstraint(FactsWriter.Fact.alertDialogFixedButtonHandler, handler, dialogNode.nodeID)
                    }
                  }
                }
              }
              if (invokedMethod.getSignature == "<com.google.ads.consent.ConsentInformation: void setConsentStatus(com.google.ads.consent.ConsentStatus)>") {
                writer.writeConstraint(FactsWriter.Fact.setStatus, handler, reached)
              }
              if (invokedMethod.getSignature == "<com.google.ads.consent.ConsentInformation: void requestConsentInfoUpdate(java.lang.String[],com.google.ads.consent.ConsentInfoUpdateListener)>") {
                val updateListenerType = invokeExpr.getArg(1).getType.asInstanceOf[RefType]
                val onConsentInfoUpdated = updateListenerType.getSootClass.getMethodUnsafe("void onConsentInfoUpdated(com.google.ads.consent.ConsentStatus)")
                if (onConsentInfoUpdated != null) {
                  writer.writeConstraint(FactsWriter.Fact.setConsetInfoUpdateHandler, handler, reached)
                }
              }
              if (invokedMethod.getSignature == "<android.os.BaseBundle: void putString(java.lang.String,java.lang.String)>") {
                (invokeExpr.getArg(0), invokeExpr.getArg(1)) match {
                  case (arg0: StringConstant, arg1: StringConstant) =>
                    if (arg0.value == "npa" && arg1.value == "1") {
                      writer.writeConstraint(FactsWriter.Fact.setNPA, handler)
                    }
                  case _ =>
                }
              }

              if (invokedMethod.getSignature == "<com.waps.AdView: void <init>(android.content.Context,android.widget.LinearLayout)>" ||
                invokedMethod.getSignature == "<com.waps.MiniAdView: void <init>(android.content.Context,android.widget.LinearLayout)>") {
                val layout = invokeExpr.getArg(1).asInstanceOf[Local]
                val aftDomain = vascoSolution.getValueBefore(stmt)
                if (aftDomain != null) {
                  for (node <- aftDomain.getViewNodes(reached, stmt, layout)) {
                    for (idName <- getIdName(node)) {
                      writer.writeConstraint(FactsWriter.Fact.adViewIdName, idName)
                    }
                  }
                }
              }

              val invokedMethodClass = invokedMethod.getDeclaringClass
              if (invokedMethodClass.getName == "android.media.AudioRecord" && invokedMethod.getName == "read") {
                writer.writeConstraint(FactsWriter.Fact.readAudio, handler, reached)
              }
              if (invokedMethodClass.getName == "java.lang.Class" && invokedMethod.getName == "getDeclaredMethod") {
                writer.writeConstraint(FactsWriter.Fact.invokesReflectiveAPI, handler, reached)
              }
              if (invokedMethodClass.getName == "com.google.ads.consent.ConsentForm" && invokedMethod.getName == "load") {
                writer.writeConstraint(FactsWriter.Fact.loadGoogleConsentForm, handler, reached)
              }
              if (invokedMethodClass.getName == "android.webkit.WebView" && invokedMethod.getName == "loadUrl") {
                var arg0 = "ANY"
                invokeExpr.getArg(0) match {
                  case strConstant: StringConstant =>
                    arg0 = strConstant.value
                  case _ =>
                }
                writer.writeConstraint(FactsWriter.Fact.loadWebViewUrl, handler, reached, arg0)
              }
              if (invokedMethodClass.getName == "com.tongqu.client.utils.Downloader" && invokedMethod.getName == "getInst") {
                writer.writeConstraint(FactsWriter.Fact.downloadApp, handler, reached)
              }
              // NOTE: the "reached" field here has a different semantics than gator version
              if (Constants.isAdMethod(invokedMethod)) {
                writer.writeConstraint(FactsWriter.Fact.showAd, handler, invokedMethod)
              }
              if (Constants.isSuspiciousAdMethod(invokedMethod)) {
                writer.writeConstraint(FactsWriter.Fact.showSuspiciousAd, handler, reached)
              }
              if (Constants.isInterstitialAdMethod(invokedMethod)) {
                writer.writeConstraint(FactsWriter.Fact.showInterstitialAd, handler, reached)
              }
              if (Constants.isSuspiciousInterstitialAdMethod(invokedMethod)) {
                writer.writeConstraint(FactsWriter.Fact.showSuspiciousInterstitialAd, handler, reached)
              }
            }
          }
        }
      }
    }
  }

  private val mainActivity = xmlParser.getMainActivity

  def getCallees(methodSig: String): List[SootMethod] = {
    Scene.v.getCallGraph.edgesOutOf(Scene.v.getMethod(methodSig)).asScala.map(_.getTgt.method).toList
  }

  def instrumentAllMethods(): Unit = {
    for (activity <- allActivities) {
      for (method <- activity.getMethods.asScala) {
        if (method.isConcrete && method.hasActiveBody) {
          // FIXME: remove the mainActivity workaround by fixing this
          // instrumentStartTimer(method)
        }
      }
    }
  }

  override def run(): Unit = {
    println("Pre-analysis time: " + Debug.v().getExecutionTime + " seconds")
    println("Mark II")
    readConfigs()

    Ic3Manager.init()

    allHandlers = initAllHandlers()

    DialogInitInstrument.run()

    // TODO: use a proper harness
    CallGraphManager.patchCallGraph()

    if (debugMode) {
      dumpCallgraph()
    }

    CallGraphManager.saveOldCallGraph()

    // IFDS must run before VASCO since VASCO depends on IFDS as pre-analysis
    runIFDS()

    DialogCreateInstrument.run(allActivities)

    instrumentAllMethods()

    writer = new FactsWriter(outputPath)

    for (service <- xmlParser.getServices.asScala) {
      if (service != null) {
        val curCls = Scene.v.getSootClassUnsafe(service)
        if (curCls != null && curCls.isConcrete) {
          writer.writeConstraint(FactsWriter.Fact.serviceClass, curCls)
          val names = curCls.getName.split("\\.")
          writer.writeConstraint(FactsWriter.Fact.serviceClassLastName, curCls, names.last)
        }
      }
    }

    // Write some constraints and prepare code for VASCO analysis
    for (handler <- allHandlers) {
      analyzeAnyHandlerPreVasco(handler)
    }

    for (activity <- allActivities) {
      analyzeActivityPreVasco(activity)
    }

    runVASCO()

    // Write more constraints
    writeConstraintsPostVASCO()

    // Dump abstractions
    if (debugMode) {
      val printWriter = new PrintWriter("/tmp/abstractions.txt")
      for (m <- analyzedMethods) {
        printWriter.println("====== Method " + m.getSignature + " =======")
        printWriter.println(m.getActiveBody)
        for (unit <- m.getActiveBody.getUnits.asScala) {
          val abstractions = ifdsSolver.ifdsResultsAt(unit)
          val aftDomain = vascoSolution.getValueAfter(unit)
          if ((abstractions != null && abstractions.size() > 0) || (aftDomain != null && aftDomain.nonEmpty)) {
            if (abstractions != null && abstractions.size() > 0) {
              for (value <- abstractions.asScala) {
                for (abstraction <- value._2) {
                  printWriter.println("\t\t" + value._1 + ": " + abstraction)
                }
              }
            }

            printWriter.println("\tUnit: " + unit)
            if (aftDomain != null && aftDomain.nonEmpty) {
              printWriter.println("AFTDomain: ")
              printWriter.println(aftDomain)
            }

            printWriter.println()
          }
        }
      }
      printWriter.close()
    }
  }

  def getActivityHandlers(activity: SootClass): Map[SootMethod, String] = {
    Constants.activityHandlerSubsigs.flatMap { case (sig, event) =>
      val c = hier.matchForVirtualDispatch(sig, activity)
      if (c != null && c.isApplicationClass) {
        Some((c.getMethod(sig), event))
      } else {
        None
      }
    }
  }

  // NOTE: a type-safe way to prevent missing run All to configure heap transfer
  private val startWindowStmts: mutable.Set[Stmt] = mutable.Set()

  def isStartWindowStmt(stmt: Stmt): Boolean = startWindowStmts.contains(stmt)
}
