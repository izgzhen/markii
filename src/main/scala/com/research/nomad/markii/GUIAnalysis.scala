/*
 * Copyright (c) 2020. Zhen Zhang
 */

package com.research.nomad.markii

import java.io.{FileWriter, PrintWriter}
import java.nio.file.{Files, Paths}

import com.research.nomad.markii.analyses.PreVASCO
import com.research.nomad.markii.dataflow.AbsNode.ViewNode
import com.research.nomad.markii.dataflow.custom.{AbsFS, FromConfig}
import com.research.nomad.markii.dataflow.{AFTDomain, AbsAttr, AbsNode, AbsValSet, AbstractValue, AbstractValuePropIFDS, AbstractValuePropVASCO, CustomStatePropVASCO, DialogButtonType}
import com.research.nomad.markii.instrument.{AllInstrument, DialogCreateInstrument, DialogInitInstrument}
import heros.InterproceduralCFG
import heros.solver.IFDSSolver
import presto.android.gui.listener.EventType
import presto.android.gui.wtg.util.WTGUtil
import presto.android.{Configs, Debug, MethodNames}
import presto.android.xml.AndroidView
import soot.jimple.{InstanceInvokeExpr, Stmt, StringConstant}
import soot.jimple.toolkits.ide.icfg.JimpleBasedInterproceduralCFG
import soot.{Local, RefType, Scene, SootClass, SootMethod, Value}
import vasco.{DataFlowSolution, Helper}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

case class Runner(method: SootMethod, loopExit: soot.Unit, view: Local)

/**
 * Core GUI Analysis that implement the client interface IAnalysis
 */
object GUIAnalysis extends IAnalysis {
  private var writer: FactsWriter = _

  private val analyzedMethods = mutable.Set[SootMethod]()
  private val analyzedHandlers = mutable.Set[SootMethod]()

  private var outputPath = "/tmp/markii-facts/"
  private var apiSemanticConfig: Option[String] = None
  private var debugMode = false
  def isDebugMode: Boolean = debugMode

  private var ifdsSolver: IFDSSolver[soot.Unit, (Value, Set[AbstractValue]), SootMethod, InterproceduralCFG[soot.Unit, SootMethod]] = _
  private var icfg: JimpleBasedInterproceduralCFG = _
  private var vascoSolution: DataFlowSolution[soot.Unit, AFTDomain] = _

  val dialogHandlerToAnalyze = mutable.Set[SootMethod]()

  private def _appendYamlReport(content: String): Unit = {
    Files.createDirectories(Paths.get(outputPath))
    val fileWriter = if (Files.exists(Paths.get(outputPath + "/report.yaml"))) {
      new FileWriter(outputPath + "/report.yaml", true)
    } else {
      new FileWriter(outputPath + "/report.yaml")
    }
    fileWriter.write(content + "\n")
    fileWriter.close()
  }

  private def appendYamlReport(key: String, value: Long): Unit = {
    _appendYamlReport(s"$key: $value")
  }

  private def appendYamlReport(key: String, value: String): Unit = {
    _appendYamlReport(s"$key: $value")
  }

  private def appendYamlReport(key: String, iterable: Iterable[String]): Unit = {
    _appendYamlReport(key + ":\n" + iterable.map(s => "    - " + s).mkString("\n"))
  }

  override def run(): Unit = {
    icfg = new JimpleBasedInterproceduralCFG()

    readConfigs() // FIXME: make it clear what data is read by this

    Files.deleteIfExists(Paths.get(outputPath + "/report.yaml"))
    appendYamlReport("pre_analysis_time_seconds", Debug.v().getExecutionTime)
    appendYamlReport("soot_get_classes_size", Scene.v().getClasses().size())
    appendYamlReport("soot_get_application_classes_size", Scene.v().getApplicationClasses().size())
    appendYamlReport("soot_get_application_classes", Scene.v().getApplicationClasses().asScala.map(_.getName))

    Ic3Manager.init()
    AppInfo.init()

    appendYamlReport("hier_application_activity_classes_size", AppInfo.hier.applicationActivityClasses.size())
    appendYamlReport("hier_application_activity_classes", AppInfo.hier.applicationActivityClasses.asScala.map(_.getName))

    DialogInitInstrument.run()

    // TODO: use a proper harness
    CallGraphManager.patchCallGraph()

    CallGraphManager.saveOldCallGraph()

    // IFDS must run before VASCO since VASCO depends on IFDS as pre-analysis
    runIFDS()

    initializeMethodMap()

    DialogCreateInstrument.run(AppInfo.allActivities)

    AllInstrument.run(AppInfo.allActivities)

    // Write some constraints and prepare code for VASCO analysis
    for (handler <- AppInfo.getAllHandlers) {
      PreVASCO.analyze(handler)
    }

    writer = new FactsWriter(outputPath)

    for (service <- AppInfo.getServices) {
      writer.writeFact(FactsWriter.Fact.serviceClass, service)
      val names = service.getName.split("\\.")
      writer.writeFact(FactsWriter.Fact.serviceClassLastName, service, names.last)
    }

    for ((handler, reached, target) <- PreVASCO.getStartActivityFacts) {
      writer.writeFact(FactsWriter.Fact.startActivity, handler, reached, target)
    }

    for (activity <- AppInfo.allActivities) {
      analyzeActivityPreVasco(activity)
    }

    runVASCO()

    // Write more constraints
    writeFactsPostVASCO()

    if (apiSemanticConfig.nonEmpty) {
      runCustomVASCO()
    }

    writer.close()

    // Dump abstractions
    if (debugMode) {
      dumpCallgraph()
      dumpIFDSAbstractions("/tmp/ifds-abstractions.txt")
      Util.dumpVASCOAbstractions[AFTDomain]("/tmp/vasco-abstractions.txt",
        vascoSolution, x => x.nonEmpty, x => x.toString, analyzedMethods)
    }
  }

  private def dumpIFDSAbstractions(outputPath: String): Unit = {
    val printWriter = new PrintWriter(outputPath)
    for (m <- analyzedMethods) {
      printWriter.println("====== Method " + m.getSignature + " =======")
      printWriter.println(m.getActiveBody)
      for (unit <- m.getActiveBody.getUnits.asScala) {
        val abstractions = ifdsSolver.ifdsResultsAt(unit)
        if (abstractions != null && abstractions.size() > 0) {
          for (value <- abstractions.asScala) {
            for (abstraction <- value._2) {
              printWriter.println("\t\t" + value._1 + ": " + abstraction)
            }
          }
          printWriter.println()
        }
      }
    }
    printWriter.close()
  }

  private def analyzeViewNode(viewNode: ViewNode): Unit = {
    viewNode.id.foreach(id => {
      AppInfo.getIdName(id) match {
        case Some(idName) =>
          writer.writeFact(FactsWriter.Fact.idName, idName, viewNode.nodeID)
          if (Constants.isAdIdName(idName)) {
            writer.writeFact(FactsWriter.Fact.adViewIdName, idName)
          }
        case None =>
      }
    })
    // TODO: analyze the mutations of default values
    var hasContentDescription = false
    for ((attrib, value) <- viewNode.getAttrs) {
      if (value != null) {
        attrib match {
          case AndroidView.ViewAttr.layout_height =>
            writer.writeDimensionFact(FactsWriter.Fact.layoutHeight, value, viewNode.nodeID)
          case AndroidView.ViewAttr.layout_width =>
            writer.writeDimensionFact(FactsWriter.Fact.layoutWidth, value, viewNode.nodeID)
          case AndroidView.ViewAttr.textSize =>
            writer.writeDimensionFact(FactsWriter.Fact.textSize, value, viewNode.nodeID)
          case AndroidView.ViewAttr.background =>
            writer.writeFact(FactsWriter.Fact.background, value, viewNode.nodeID)
          case AndroidView.ViewAttr.text =>
            writer.writeFact(FactsWriter.Fact.textContent, value, viewNode.nodeID)
            if (value.toLowerCase().contains("rec")) {
              writer.writeFact(FactsWriter.Fact.recordButton, viewNode.nodeID)
            }
            if (value.toLowerCase().contains("continue")) {
              writer.writeFact(FactsWriter.Fact.actionButton, viewNode.nodeID)
            }
          case AndroidView.ViewAttr.dialogTitle =>
            writer.writeFact(FactsWriter.Fact.dialogTitle, value, viewNode.nodeID)
          case AndroidView.ViewAttr.dialogMessage =>
            writer.writeFact(FactsWriter.Fact.dialogMessage, value, viewNode.nodeID)
          case AndroidView.ViewAttr.contentDescription => hasContentDescription = true
          case _ =>
        }
      }
    }
    for ((attrib, value) <- viewNode.getAppAttrs) {
      if (value != null) {
        writer.writeFact(FactsWriter.Fact.withName(attrib.name()), viewNode.nodeID, value)
      }
    }
    viewNode.sootClass match {
      case Some(c) =>
        if (!hasContentDescription && AppInfo.hier.isSubclassOf(c, Constants.imageViewClass)) {
          writer.writeFact(FactsWriter.Fact.imageHasNoContentDescription, viewNode.nodeID)
        }
        if (Constants.isAdViewClass(c)) {
          writer.writeFact(FactsWriter.Fact.adViewClass, c)
        }
        writer.writeFact(FactsWriter.Fact.viewClass, c.getName, viewNode.nodeID)
        if (AppInfo.hier.isSubclassOf(c, Constants.buttonViewClass)) writer.writeFact(FactsWriter.Fact.buttonView, viewNode.nodeID)
        if (Constants.isDialogClass(c)) {
          writer.writeFact(FactsWriter.Fact.dialogView, viewNode.nodeID, getMethodOf(viewNode.allocSite))
        }
      case None =>
    }
  }

  private val unitToMethodMap: mutable.Map[soot.Unit, SootMethod] = mutable.Map[soot.Unit, SootMethod]()
  private def initializeMethodMap(): Unit = {
    for (c <- Scene.v().getClasses.asScala) {
      for (m <- c.getMethods.asScala) {
        if (m.hasActiveBody) {
          for (u <- m.getActiveBody.getUnits.asScala) {
            unitToMethodMap.addOne(u, m)
          }
        }
      }
    }
  }

  def getMethodOf(stmt: Stmt): SootMethod = {
    unitToMethodMap(stmt)
  }

  private def analyzeActivityHandlerPostVasco(handler: SootMethod): Unit = {
    for (endpoint <- icfg.getEndPointsOf(handler).asScala) {
      val aftDomain = vascoSolution.getValueAfter(endpoint)
      if (aftDomain != null) {
        // TODO: inspect view nodes at the end of each activity handler
        // NOTE: there is extra computation, but do we care?
        for ((node, children) <- aftDomain.nodeEdgeMap) {
          val ownerActivities = aftDomain.getOwnerActivities(handler, endpoint.asInstanceOf[Stmt], node)
          analyzeViewNode(node)
          for (child <- children) {
            writer.writeFact(FactsWriter.Fact.containsView, node.nodeID, child.nodeID)
            analyzeViewNode(child)
          }
        }
      }
    }
  }

  private def dumpCallgraph(): Unit = {
    val printWriter: PrintWriter = new PrintWriter("/tmp/call-graph.txt")
    for (edge <- Scene.v().getCallGraph.asScala) {
      val source = edge.getSrc.method()
      if (analyzedMethods.contains(source) && !Configs.isLibraryClass(source.getDeclaringClass.getName)) {
        printWriter.println(edge.getSrc + "\n ==> " + edge.getTgt + "\n")
      }
    }
    printWriter.close()
  }


  private def runVASCO(): Unit = {
    // NOTE: over-approx of entrypoints
    val entrypointsFull = AppInfo.allActivities.flatMap(DynamicCFG.getRunner).map(_.method).toList
    val vascoProp = new AbstractValuePropVASCO(entrypointsFull)
    println("VASCO starts")
    vascoProp.doAnalysis()
    println("VASCO finishes")

    analyzedMethods.addAll(AppInfo.getAllHandlers)
    analyzedMethods.addAll(vascoProp.getMethods.asScala)
    if (sys.env.contains("BATCH_RUN")) {
      vascoSolution = Helper.getMeetOverValidPathsSolution(vascoProp)
    } else {
      vascoSolution = Helper.getMeetOverValidPathsSolutionPar(vascoProp)
    }
    println("VASCO solution generated")
  }

  private def runCustomVASCO(): Unit = {
    // NOTE: over-approx of entrypoints
    // FIXME: code duplication
    val entrypointsFull = AppInfo.allActivities.flatMap(DynamicCFG.getRunner).map(_.method).toList

    val eventHandlers =
      writer.getStoredFacts(FactsWriter.Fact.eventHandler).map(
        args => (args(1).asInstanceOf[SootMethod], args.head.asInstanceOf[EventType])).toMap ++
      writer.getStoredFacts(FactsWriter.Fact.enqueuedWorkRequest).map(
        args => (args(2).asInstanceOf[SootMethod], EventType.implicit_time_tick)).toMap

    val fromConfig = new FromConfig(apiSemanticConfig.get)
    val vascoProp = new CustomStatePropVASCO[AbsValSet[AbsFS]](entrypointsFull ++ eventHandlers.keys.toList, fromConfig)
    println("VASCO starts")
    vascoProp.doAnalysis()
    println("VASCO finishes")

    for ((handler, eventType) <- eventHandlers) {
      val reachedMethods = CallGraphManager.reachableMethods(handler)
      for (reached <- reachedMethods) {
        if (reached.getDeclaringClass.isApplicationClass && reached.isConcrete && reached.hasActiveBody) {
          for ((ref, prevState, postState) <- fromConfig.getTransitions(reached)) {
            val refStr = ref match {
              case Some(r) => r.toString
              case None => "unknown"
            }
            writer.writeFact(FactsWriter.Fact.apiStateTransition, handler, eventType, reached, refStr, prevState, postState)
          }
        }
      }
    }
  }

  private def readConfigs(): Unit = {
    for (param <- Configs.clientParams.asScala) {
      if (param.startsWith("output:")) outputPath = param.substring("output:".length)
      if (param.startsWith("apiSemanticConfig:")) apiSemanticConfig = Some(param.substring("apiSemanticConfig:".length))
    }

    debugMode = Configs.clientParams.contains("debugMode:true")
  }

  private def runIFDS(): Unit = {
    val analysis = new AbstractValuePropIFDS(icfg)
    ifdsSolver = new IFDSSolver(analysis)
    System.out.println("======================== IFDS Solver started  ========================")
    val entrypoints = Scene.v().getEntryPoints.asScala.filter(m => {
      val c = m.getDeclaringClass
      Constants.isActivity(c) || Constants.isService(c) || Constants.isReceiver(c)
    })
    Scene.v().setEntryPoints(entrypoints.asJava)

    appendYamlReport("ifds_entrypoints_size", Scene.v().getEntryPoints.size())
    appendYamlReport("ifds_entrypoints", Scene.v().getEntryPoints.asScala.map(_.getSignature))

    ifdsSolver.solve()
    analyzedMethods.addAll(analysis.visitedMethods.asScala)
    System.out.println("======================== IFDS Solver finished ========================")
  }

  private def writeFactsPostVASCO(): Unit = {
    for (act <- AppInfo.allActivities) {
      for ((handler, event) <- AppInfo.getActivityHandlers(act)) {
        analyzeAnyHandlerPostVASCO(handler)
        writer.writeFact(FactsWriter.Fact.activityEventHandler, event, handler, act)
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

    for (handler <- AppInfo.getAllHandlers) {
      analyzeAnyHandlerPostVASCO(handler)
    }

    for (method <- dialogHandlerToAnalyze) {
      analyzeActivityHandlerPostVasco(method)
    }

    if (AppInfo.mainActivity != null) {
      writer.writeFact(FactsWriter.Fact.mainActivity, AppInfo.mainActivity)
    }
    val newlpMainActivity = Scene.v.getSootClassUnsafe("com.revlwp.wallpaper.newlp.MainActivity")
    if (newlpMainActivity != null) {
      writer.writeFact(FactsWriter.Fact.mainActivity, newlpMainActivity)
    }

    for ((action, m) <- AppInfo.getIntents) {
      writer.writeFact(FactsWriter.Fact.intentFilter, action, m)
    }
  }

  private def analyzeActivityPreVasco(activityClass: SootClass): Unit = {
    for ((handler, _) <- AppInfo.getActivityHandlers(activityClass)) {
      DynamicCFG.addActivityHandlerToEventLoop(activityClass, handler)
    }
    val onCreate = AppInfo.hier.virtualDispatch(MethodNames.onActivityCreateSubSig, activityClass)
    if (onCreate != null) {
      writer.writeFact(FactsWriter.Fact.lifecycleMethod, activityClass, "onCreate", onCreate)
    }

    val onDestroy = AppInfo.hier.virtualDispatch(MethodNames.onActivityDestroySubSig, activityClass)
    if (onDestroy != null) {
      writer.writeFact(FactsWriter.Fact.lifecycleMethod, activityClass, "onDestroy", onDestroy)
    }
    if (AppInfo.hier.isSubclassOf(activityClass, Constants.prefActivity)) {
      writer.writeFact(FactsWriter.Fact.preferenceActivity, activityClass)
    }
  }

  private def analyzeAnyHandlerPostVASCO(handler: SootMethod): Unit = {
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
          for ((eventHandler, sourceLoc) <- eventHandlers) {
            writer.writeFact(FactsWriter.Fact.eventHandler, eventType, eventHandler, viewNode.nodeID)
            writer.writeFact(FactsWriter.Fact.methodLineNumber,
              eventHandler,
              sourceLoc.file,
              sourceLoc.lineNumber,
              eventHandler.getDeclaringClass,
              viewNode.nodeID)
            analyzeAnyHandlerPostVASCO(eventHandler)
          }
        }
        for ((act, nodes) <- aftDomain.activityRootViewMap) {
          for (viewNode <- nodes) {
            writer.writeFact(FactsWriter.Fact.rootView, act, viewNode.nodeID)
          }
        }
      } else {
        println("[WARN] Please submit an issue at https://github.com/izgzhen/markii/issues/new if this handler is missing " + handler + ":")
        println("[WARN] Title: Miss handler in vascoSolution")
        println("[WARN] Body:")
        println(s"[WARN] Handler signature: ${handler.getSignature}")
        println(s"[WARN] APK path or APK compilation steps: [PLEASE FILL]")
        println(s"[WARN] Other steps to reproduce this error: [PLEASE FILL]")
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
              writer.writeFact(FactsWriter.Fact.startActivity, handler, reached, targetAct)
            }
            if (intent.getAction != null && intent.getAction.equals("android.intent.action.VIEW")) {
              if (intent.getDataScheme != null && intent.getDataScheme.equals("market")) {
                writer.writeFact(FactsWriter.Fact.startViewActivityOfMarketHost, handler, reached);
              } else {
                writer.writeFact(FactsWriter.Fact.startViewActivityOfSomeHosts, handler, reached);
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
                  writer.writeFact(FactsWriter.Fact.finishActivity, handler, reached, actClass)
                case _ =>
              }
            }
            if (invokedMethod != null) {
              if (Constants.isDialogDismiss(invokedMethod.getSignature)) {
                val dialog = invokeExpr.asInstanceOf[InstanceInvokeExpr].getBase.asInstanceOf[Local]
                if (aftDomain != null) {
                  // NOTE: We can combine the for-loops here
                  for (dialogNode <- aftDomain.getViewNodes(reached, stmt, dialog)) {
                    writer.writeFact(FactsWriter.Fact.dismiss, handler, dialogNode.nodeID)
                  }
                }
              }
              // FIXME: frauddroid-gba.apk "<android.app.Activity: void showDialog(int)>" and onPrepareDialog, onCreateDialog
              // 1. localNodeMap should include the dialogNode that was initialized in previous lines
              // 2. iterate dialogHandlerToAnalyze
              if (aftDomain != null) {
                PreVASCO.getShowDialogInvocations(stmt) match {
                  case Some(dialogBase) => {
                    for (dialogNode <- aftDomain.getViewNodes(reached, stmt, dialogBase)) {
                      writer.writeFact(FactsWriter.Fact.dialogView, dialogNode.nodeID, getMethodOf(dialogNode.allocSite))
                      writer.writeFact(FactsWriter.Fact.showDialog, handler, reached, dialogNode.nodeID)
                      // Write fact about dialog's YES/NO buttons' handlers
                      for (buttonType <- DialogButtonType.values) {
                        for (childNode <- aftDomain.findViewByButtonType(dialogNode, buttonType)) {
                          writer.writeFact(FactsWriter.Fact.dialogViewButton, dialogNode.nodeID,
                            childNode.nodeID, buttonType.toString)
                        }
                      }
                    }
                  }
                  case _ =>
                }
              }
              for(dialogHandler <- dialogHandlerToAnalyze){
                analyzeAnyHandlerPostVASCO(dialogHandler)
              }
              if (invokedMethod.getSignature == "<com.google.ads.consent.ConsentInformation: void setConsentStatus(com.google.ads.consent.ConsentStatus)>") {
                writer.writeFact(FactsWriter.Fact.setStatus, handler, reached)
              }
              if (invokedMethod.getSignature == "<com.google.ads.consent.ConsentInformation: void requestConsentInfoUpdate(java.lang.String[],com.google.ads.consent.ConsentInfoUpdateListener)>") {
                val updateListenerType = invokeExpr.getArg(1).getType.asInstanceOf[RefType]
                val onConsentInfoUpdated = updateListenerType.getSootClass.getMethodUnsafe("void onConsentInfoUpdated(com.google.ads.consent.ConsentStatus)")
                if (onConsentInfoUpdated != null) {
                  writer.writeFact(FactsWriter.Fact.setConsetInfoUpdateHandler, handler, reached)
                }
              }
              if (invokedMethod.getSignature == "<android.os.BaseBundle: void putString(java.lang.String,java.lang.String)>") {
                (invokeExpr.getArg(0), invokeExpr.getArg(1)) match {
                  case (arg0: StringConstant, arg1: StringConstant) =>
                    if (arg0.value == "npa" && arg1.value == "1") {
                      writer.writeFact(FactsWriter.Fact.setNPA, handler)
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
                    for (idName <- AppInfo.getIdName(node)) {
                      writer.writeFact(FactsWriter.Fact.adViewIdName, idName)
                    }
                  }
                }
              }

              val invokedMethodClass = invokedMethod.getDeclaringClass
              if (invokedMethodClass.getName == "android.media.AudioRecord" && invokedMethod.getName == "read") {
                writer.writeFact(FactsWriter.Fact.readAudio, handler, reached)
              }
              if (invokedMethodClass.getName == "java.lang.Class" && invokedMethod.getName == "getDeclaredMethod") {
                writer.writeFact(FactsWriter.Fact.invokesReflectiveAPI, handler, reached)
              }
              if (invokedMethodClass.getName == "com.google.ads.consent.ConsentForm" && invokedMethod.getName == "load") {
                writer.writeFact(FactsWriter.Fact.loadGoogleConsentForm, handler, reached)
              }
              if (invokedMethodClass.getName == "androidx.work.WorkManager" && invokedMethod.getName == "enqueue") {
                val request = invokeExpr.getArg(0).asInstanceOf[Local]
                val aftDomain = vascoSolution.getValueBefore(stmt)
                if (aftDomain != null) {
                  for (node <- aftDomain.getNodes(reached, stmt, request)) {
                    node match {
                      case AbsNode.UnifiedObjectNode(absName, absAttrs) =>
                        val doWork = absAttrs("workerClass").asInstanceOf[AbsAttr.ClassVal].c.getMethodByName("doWork")
                        writer.writeFact(FactsWriter.Fact.withName("enqueued" + absName), handler, reached, doWork)
                        analyzeAnyHandlerPostVASCO(doWork)
                    }
                  }
                }
              }
              if (invokedMethodClass.getName == "android.webkit.WebView" && invokedMethod.getName == "loadUrl") {
                var arg0 = "ANY"
                invokeExpr.getArg(0) match {
                  case strConstant: StringConstant =>
                    arg0 = strConstant.value
                  case _ =>
                }
                writer.writeFact(FactsWriter.Fact.loadWebViewUrl, handler, reached, arg0)
              }
              if (invokedMethodClass.getName == "com.tongqu.client.utils.Downloader" && invokedMethod.getName == "getInst") {
                writer.writeFact(FactsWriter.Fact.downloadApp, handler, reached)
              }
              // NOTE: the "reached" field here has a different semantics than gator version
              if (Constants.isAdMethod(invokedMethod)) {
                writer.writeFact(FactsWriter.Fact.showAd, handler, invokedMethod)
              }
              if (Constants.isSuspiciousAdMethod(invokedMethod)) {
                writer.writeFact(FactsWriter.Fact.showSuspiciousAd, handler, reached)
              }
              if (Constants.isInterstitialAdMethod(invokedMethod)) {
                writer.writeFact(FactsWriter.Fact.showInterstitialAd, handler, reached)
              }
              if (Constants.isSuspiciousInterstitialAdMethod(invokedMethod)) {
                writer.writeFact(FactsWriter.Fact.showSuspiciousInterstitialAd, handler, reached)
              }
            }
          }
        }
      }
    }
  }

  def getIfdsResultAt(stmt: Stmt, target: Value): Iterable[AbstractValue] = {
    ifdsSolver.ifdsResultsAt(stmt).asScala.flatMap { case (tainted, taints) =>
      if (tainted.equivTo(target)) {
        taints
      } else {
        Set()
      }
    }
  }
}
