/*
 * Copyright (c) 2020. Zhen Zhang
 */

package com.research.nomad.markii.dataflow

import java.time.Instant

import com.research.nomad.markii.analyses.PreVASCO
import com.research.nomad.markii.dataflow.AbsNode.{ActNode, ListenerNode, ViewNode}
import com.research.nomad.markii.{AppInfo, Constants, DynamicCFG, GUIAnalysis, Util}
import io.github.izgzhen.msbase.{IOUtil, JsonUtil}
import presto.android.gui.listener.EventType

import scala.jdk.CollectionConverters._
import soot.jimple.internal.{JIdentityStmt, JimpleLocal}
import soot.jimple.{AssignStmt, CastExpr, InstanceFieldRef, InstanceInvokeExpr, IntConstant, InvokeExpr, NewExpr, ReturnStmt, StaticFieldRef, Stmt, ThisRef}
import soot.{Local, RefType, Scene, SootClass, SootMethod, Value}
import vasco.{ForwardInterProceduralAnalysis, ProgramRepresentation, VascoContext}

import scala.collection.mutable

/**
 * Implementation of abstract value propagation for VASCO DFA framework
 * @param entryPoints: entry point methods
 */
class AbstractValuePropVASCO(entryPoints: List[SootMethod])
  extends ForwardInterProceduralAnalysis[SootMethod, soot.Unit, AFTDomain] {

  type Domain = AFTDomain
  type DomainContext = VascoContext[SootMethod, soot.Unit, Domain]

  private val RETURN_LOCAL = new JimpleLocal("@return", null)

  private def setDialogButtonListener(buttonType: DialogButtonType.Value, invokeExpr: InvokeExpr, d: AFTDomain,
                                      ctxMethod: SootMethod, stmt: Stmt): AFTDomain = {
    invokeExpr.getArgs.asScala.map(_.getType).collectFirst {
      case refType: RefType if Constants.isDialogOnClickListener(refType.getSootClass) => refType.getSootClass
    } match {
      case Some(listener) =>
        val handler = listener.getMethodByNameUnsafe("onClick")
        if (handler != null) {
          val dialogBuilder = invokeExpr.asInstanceOf[InstanceInvokeExpr].getBase.asInstanceOf[Local]
          d.setDialogButtonHandler(ctxMethod, stmt, dialogBuilder, handler, Some(buttonType))
        } else {
          d
        }
      case _ => d
    }
  }

  def assigned(context: DomainContext, assignStmt: Stmt, ref: Ref,
               value: Value, ctx: Domain, input: Domain): Domain = {
    val killed = input.killRef(ref)
    val ctxMethod = context.getMethod
    value match {
      case castExpr: CastExpr =>
        assigned(context, assignStmt, ref, castExpr.getOp, ctx, input)
      case newExpr: NewExpr =>
        val sootClass = newExpr.getBaseType.getSootClass
        if (AppInfo.hier.isSubclassOf(sootClass, Scene.v().getSootClass(Constants.androidViewClassName))) {
          killed.newView(ref, sootClass, assignStmt)
        } else if (Constants.isDialogBuilderClass(sootClass)) {
          killed.newView(ref, sootClass, assignStmt)
        } else if (Constants.isDialogClass(sootClass)) {
          killed.newView(ref, sootClass, assignStmt)
        } else if (Constants.isViewEventListenerClass(sootClass)) {
          killed.newListener(ref, sootClass)
        } else if (AppInfo.hier.isSubclassOf(sootClass, Constants.layoutParamsClass)) {
          killed.newLayoutParams(ref)
        } else if (UnifiedObjectAPI.isBaseClass(sootClass)) {
          killed.newUnifiedObject(ref, sootClass, assignStmt)
        } else {
          killed
        }
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
        val signature = invokeExpr.getMethod.getSignature
        if (Constants.isActivityFindViewById(invokeExpr.getMethod)) {
          invokeExpr.getArg(0) match {
            case intConstant: IntConstant =>
              val activityBase = invokeExpr.asInstanceOf[InstanceInvokeExpr].getBase
              // FIXME: over-approx of activity instances
              val activityClass = activityBase.getType.asInstanceOf[RefType].getSootClass
              val subViews = mutable.Set[ViewNode]()
              val id = intConstant.value
              ctx.activityRootViewMap.get(activityClass) match {
                case Some(rootViews) =>
                  for (viewNode <- rootViews) {
                    subViews.addAll(ctx.findViewById(viewNode, id))
                  }
                case _ =>
              }
              return killed.withNodes(ref, subViews.toSet)
            case _ =>
          }
        }
        if (Constants.isDialogFindViewById(invokeExpr.getMethod) || Constants.isViewFindViewById(invokeExpr.getMethod)) {
          invokeExpr.getArg(0) match {
            case intConstant: IntConstant =>
              val viewLocal = invokeExpr.asInstanceOf[InstanceInvokeExpr].getBase.asInstanceOf[Local]
              val subViews = mutable.Set[ViewNode]()
              val id = intConstant.value
              for (viewNode <- ctx.getViewNodes(ctxMethod, assignStmt, viewLocal)) {
                subViews.addAll(ctx.findViewById(viewNode, id))
              }
              return killed.withNodes(ref, subViews.toSet)
            case _ =>
          }
        }
        if (invokeExpr.getMethod.getName == "findViewById") {
          println("Unhandled signature: " + signature)
        }
        if (Constants.isDialogBuilderCreate(signature)) {
          val builder = invokeExpr.asInstanceOf[InstanceInvokeExpr].getBase.asInstanceOf[Local]
          return killed.dialogCreate(
            ref, sootClass = invokeExpr.getType.asInstanceOf[RefType].getSootClass,
            assignStmt, builder, ctxMethod)
        }
        val subSig = invokeExpr.getMethod.getSubSignature
        val setButtionType = Constants.fromDialogBuilderSetButton(subSig)
        if (setButtionType.nonEmpty) {
          setDialogButtonListener(setButtionType.get, invokeExpr, input, ctxMethod, assignStmt)
        } else if (Constants.isDialogBuilderSetAny(subSig)) {
          input // Don't kill for other builder setters
        } else if (signature == "<android.view.LayoutInflater: android.view.View inflate(int,android.view.ViewGroup,boolean)>") {
          // FIXME: other inflate signatures
          invokeExpr.getArg(0) match {
            case intConstant: IntConstant =>
              invokeExpr.getArg(1) match {
                case parentView: Local =>
                  val attachToRoot = invokeExpr.getArg(2).asInstanceOf[IntConstant].value != 0
                  inflate(killed, context, assignStmt, intConstant.value, ref, parentView, attachToRoot)
                case _ => killed
              }
            case _ => killed
          }
        } else if (invokeExpr.isInstanceOf[InstanceInvokeExpr] &&
                   UnifiedObjectAPI.isBaseClass(invokeExpr.getMethod.getDeclaringClass)) {
          // TODO: rewrite this whole thing with CustomState, use alias and analyze semantics
          val base = invokeExpr.asInstanceOf[InstanceInvokeExpr].getBase
          assigned(context, assignStmt, ref, base, ctx, input)
        } else {
          killed
        }
      case _ => killed
    }
  }

//  private var maxLog = Some(100)
  private var maxLog: Option[Int] = None
  private var logStateCounter = 0
  private var logStateMethodCounters = mutable.Map[SootMethod, Int]()
  private val logStateInterval = 1000

  private def logState(method: SootMethod, unit: soot.Unit, d: Domain): Unit = {
    if (!GUIAnalysis.isDebugMode) return
    if (logStateCounter == logStateInterval) {
      println(logStateMethodCounters.toList.sortBy(_._2).takeRight(5))
      logStateMethodCounters = mutable.Map[SootMethod, Int]()
      logStateCounter = 0
      println(s"worklist size: ${Instant.now.getEpochSecond}: ${worklist.size()}")
    } else {
      logStateCounter += 1
      logStateMethodCounters(method) = logStateMethodCounters.getOrElse(method, 0) + 1
    }
    if (method.getSignature != "<xvideo.furbie.ro.AsyncListActivity: void gopro()>") {
      return
    }
    if (maxLog.nonEmpty) {
      if (maxLog.get < 0) {
        throw new RuntimeException("Logging ends")
      } else {
        maxLog = Some(maxLog.get - 1)
      }
    }
    val idx = Util.getUnitIndex(method.getActiveBody.getUnits, unit)
    val obj = Map(
      "method" -> method.getSignature,
      "unit" -> unit.toString(),
      "unitIdx" -> idx,
      "domain" -> d.toJSONObj,
      "domainSize" -> d.sizeSummary
    )
    val json = JsonUtil.toJson(obj)
    IOUtil.write(json, "/tmp/markii-debug/" + method.getSignature + "@" + idx + ".json")
  }

  override def normalFlowFunction(context: DomainContext, unit: soot.Unit, d: Domain): Domain = {
    logState(context.getMethod, unit, d)
    unit match {
      case assignStmt: AssignStmt =>
        val ref = Ref.from(assignStmt.getLeftOp)
        return assigned(context, assignStmt, ref, assignStmt.getRightOp, d, d)
      case stmt: JIdentityStmt =>
        val methodName = context.getMethod.getName
        val methodClass = context.getMethod.getDeclaringClass
        if (stmt.getRightOp.isInstanceOf[ThisRef] && Constants.isActivity(methodClass) && methodName == Constants.runnerMethodName) {
          // HACK: This is a HACK
          val localRef = Ref.from(stmt.getLeftOp)
          val killed = d.killRef(localRef)
          return killed.newAct(localRef, methodClass)
        }
      case retStmt: ReturnStmt =>
        return assigned(context, retStmt, Ref.LocalRef(RETURN_LOCAL), retStmt.getOp, d, d)
      case _ =>
    }
    d
  }

  // <com.appbrain.AppBrainActivity: boolean onKeyDown(int,android.view.KeyEvent)> is not entered, taken as a local call instead
  // APK: examples/frauddroid-autumn.apk
  override def callEntryFlowFunction(context: DomainContext, m: SootMethod, callSite: soot.Unit, d: Domain): Domain = {
    val stmt = callSite.asInstanceOf[Stmt]
    val invokeExpr = stmt.getInvokeExpr
    var ret = if (PreVASCO.isStartWindowStmt(callSite.asInstanceOf[Stmt])) {
      topValue()
    } else {
      d.getHeap
    }
    // Set the current window class in the abstract state before entering into the lifecycle method
    ret = Constants.getLifecycleMethodWindow(m) match {
      case Some(cls) => ret.copy(currentWindowClasses = Set(cls))
      case None => ret
    }
    if (m.hasActiveBody) {
      for (i <- 0 until invokeExpr.getArgCount) {
        ret = assigned(context, stmt, Ref.LocalRef(m.getActiveBody.getParameterLocal(i)), invokeExpr.getArg(i), d, ret)
      }
      invokeExpr match {
        case instanceInvokeExpr: InstanceInvokeExpr =>
          ret = assigned(context, stmt, Ref.LocalRef(m.getActiveBody.getThisLocal), instanceInvokeExpr.getBase, d, ret)
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
        ret = assigned(context, assignStmt, Ref.LocalRef(assignStmt.getLeftOp.asInstanceOf[Local]), RETURN_LOCAL, d, ret)
      case _ =>
    }
    unit.asInstanceOf[Stmt].getInvokeExpr match {
      case instanceInvokeExpr: InstanceInvokeExpr =>
        val base = instanceInvokeExpr.getBase.asInstanceOf[Local]
        if (instanceInvokeExpr.getMethod.hasActiveBody) {
          val thisLocal = instanceInvokeExpr.getMethod.getActiveBody.getThisLocal
          ret = assigned(context, unit.asInstanceOf[Stmt], Ref.LocalRef(base), thisLocal, d, ret)
        }
      case _ =>
    }
    ret
  }

  /**
   * Traverse sub-nodes in an inflated abstract view tree
   */
  class InflateTraverser(window: Option[SootClass]) extends Traverser {
    override def withSubNode(d: AFTDomain, viewNode: ViewNode): AFTDomain = {
      var newDomain = d
      for ((eventType, methodInfo) <- viewNode.getInlineClickHandlers) {
        val windowClasses = window match {
          case Some(act) => List(act)
          case None => d.currentWindowClasses
        }
        for (cls <- windowClasses) {
          val method = cls.getMethodByNameUnsafe(methodInfo.getName)
          newDomain = setEventHandler(newDomain, cls, method, eventType, viewNode)
        }
      }
      newDomain
    }
  }

    /**
   * Compute the post-state after "inflate" invocation on pre-state (d)
   *
   * Reference: https://developer.android.com/reference/android/view/LayoutInflater#inflate(int,%20android.view.ViewGroup)
   */
  private def inflate(d: AFTDomain, context: DomainContext, stmt: Stmt, id: Int,
                      ref: Ref, parentView: Local, attachToRoot: Boolean): AFTDomain = {
    val view = AppInfo.findViewById(id)
    val viewNode = ViewNode(stmt, id = Set(id), androidView = view)
    val traverser = new InflateTraverser(None)
    // Set XML-declared handlers for each sub-node of the inflated view tree in the abstract state
    val newDomain = traverser.withSubNode(d, viewNode).inflateAFT(stmt, viewNode, view, Some(traverser)).withNode(ref, viewNode)
    if (attachToRoot) {
      newDomain.withEdge(context.getMethod, stmt, parentView, viewNode)
    } else {
      newDomain
    }
  }

  /**
   * NOTE: this is separated from AFTDomain.setContentViewDialog since it accesses the AndroidView -- which we need to
   * access the bind handlers including the sub-view during construction
   */
  def setContentViewDialog(d: AFTDomain, ctxMethod: SootMethod, stmt: Stmt, dialogLocal: Local, id: Int): AFTDomain = {
    val androidView = AppInfo.findViewById(id)
    val viewNode = ViewNode(stmt, id = Set(id, androidView.getId.toInt), androidView = androidView)
    d.inflateAFT(stmt, viewNode, androidView, optTraverser = Some(new InflateTraverser(None))).withEdge(ctxMethod, stmt, dialogLocal, viewNode)
  }

  /**
   * NOTE: this is separated from AFTDomain.setContentViewAct since it accesses the AndroidView -- which we need to
   * access the bind handlers including the sub-view during construction
   */
  def setContentViewAct(context: DomainContext, d: AFTDomain, stmt: Stmt, actClass: SootClass, id: Int): AFTDomain = {
    val view = AppInfo.findViewById(id)
    val viewNode = ViewNode(stmt, id = Set(id, view.getId.toInt), androidView = view)
    d.inflateAFT(stmt, viewNode, view, optTraverser = Some(new InflateTraverser(Some(actClass)))).copy(
      activityRootViewMap = d.activityRootViewMap + (actClass -> Set(viewNode))
    )
  }

  /**
   * Compute the post-state after invocation of API that set event listener (first argument of the invocation)
   * of a view node (base instance of the invocation).
   *
   * Note that the listener is an instance of a class with event handler method(s).
   */
  def setEventListener(d: AFTDomain, context: DomainContext, invokeExpr: InstanceInvokeExpr,
                       stmt: Stmt, handlerSubSig: String, eventType: EventType): AFTDomain = {
    val viewBase = invokeExpr.getBase.asInstanceOf[Local]
    var listeners: Set[SootClass] = invokeExpr.getArg(0) match {
      case local: Local =>
        d.getNodes(context.getMethod, stmt, local).collect {
          // FIXME: sometimes, the listener returned from here is not valid
          case ListenerNode(listener) => listener
        }.toSet
      case _ => Set()
    }
    invokeExpr.getArg(0).getType match {
      case refType: RefType =>
        listeners = listeners + refType.getSootClass
      case _ =>
    }
    val handlers = listeners.map(cls => {
      cls.getMethodUnsafe(handlerSubSig)
    }).filter(m => m != null && m.isConcrete && m.hasActiveBody)
    for (handler <- handlers) {
      for (dialogNode <- d.getOwnerDialogs(context.getMethod, stmt, viewBase)) {
        DynamicCFG.addViewHandlerToEventLoopDialog(dialogNode, handler) match {
          case Some((runner, invocation)) =>
            aftProgramRepresentation.refreshCFGcache(runner)
            val callers = getCallers(context)
            if (callers != null) {
              for (caller <- callers.asScala) {
                caller.getCallingContext.setValueBefore(invocation, topValue())
                caller.getCallingContext.setValueAfter(invocation, topValue())
                initContext(runner, topValue())
              }
            }
          case None =>
        }
      }

      for (ownerActivity <- d.getOwnerActivities(context.getMethod, stmt, viewBase)) {
        // FIXME: continueButton has no owner activities
        // FIXME: adding handler invocation might make some context missing nodes when joining over feasible
        //        paths
        DynamicCFG.addViewHandlerToEventLoopAct(ownerActivity, handler) match {
          case Some((runner, invocation)) =>
            aftProgramRepresentation.refreshCFGcache(runner)
            for (runnerContext <- getContexts(runner).asScala) {
              runnerContext.setValueBefore(invocation, topValue())
              runnerContext.setValueAfter(invocation, topValue())
            }
            initContext(runner, topValue())
          case None =>
        }
      }
    }
    d.setHandlers(context.getMethod, stmt, viewBase, eventType, handlers)
  }

  /**
   * Add handler to the call-graph and abstract state
   */
  def setEventHandler(d: AFTDomain, windowClass: SootClass,
                      handler: SootMethod, eventType: EventType, viewNode: ViewNode): AFTDomain = {
    // FIXME: windowClass vs Activity? Maybe we can unify them?....since the virtual/fake method contains the constructed
    //  instance for all em'
    DynamicCFG.addViewHandlerToEventLoopAct(windowClass, handler) match {
      case Some((runner, invocation)) =>
        aftProgramRepresentation.refreshCFGcache(runner)
        for (runnerContext <- getContexts(runner).asScala) {
          runnerContext.setValueBefore(invocation, topValue())
          runnerContext.setValueAfter(invocation, topValue())
        }
        initContext(runner, topValue())
      case None =>
    }
    d.addHandler(viewNode, eventType, handler)
  }

  override def callLocalFlowFunction(context: DomainContext, unit: soot.Unit, d: Domain): Domain = {
    logState(context.getMethod, unit, d)
    unit match {
      case assignStmt: AssignStmt =>
        val ref = Ref.from(assignStmt.getLeftOp)
        assigned(context, assignStmt, ref, assignStmt.getRightOp, d, d)
      case _ =>
        val stmt = unit.asInstanceOf[Stmt]
        if (stmt.containsInvokeExpr()) {
          val invokeExpr = stmt.getInvokeExpr
          if (invokeExpr.getMethod.getName == "addView" &&
            invokeExpr.getMethod.getDeclaringClass.getName == "android.view.ViewGroup") {
            val parentView = invokeExpr.asInstanceOf[InstanceInvokeExpr].getBase
            val childView = invokeExpr.getArg(0)
            return d.withEdge(context.getMethod, stmt, parentView.asInstanceOf[Local], childView.asInstanceOf[Local])
          }
          val subSig = invokeExpr.getMethod.getSubSignature
          val setButtionType = Constants.fromDialogBuilderSetButton(subSig)
          if (setButtionType.nonEmpty) {
            return setDialogButtonListener(setButtionType.get, invokeExpr, d, context.getMethod, stmt)
          }
          if (Constants.isDialogSetButton(invokeExpr.getMethod.getSignature)) {
            invokeExpr.getArgs.asScala.map(_.getType).collectFirst {
              case refType: RefType if Constants.isDialogOnClickListener(refType.getSootClass) => refType.getSootClass
            } match {
              case Some(listener) =>
                val handler = listener.getMethodByNameUnsafe("onClick")
                if (handler != null) {
                  val local = invokeExpr.asInstanceOf[InstanceInvokeExpr].getBase.asInstanceOf[Local]
                  return d.setDialogButtonHandler(context.getMethod, stmt, local, handler, None)
                }
              case _ =>
            }
          }
          if (invokeExpr.getMethod.getSubSignature == "void setOnClickListener(android.view.View$OnClickListener)") {
            return setEventListener(d, context, invokeExpr.asInstanceOf[InstanceInvokeExpr], stmt, "void onClick(android.view.View)", EventType.click)
          }
          if (invokeExpr.getMethod.getSubSignature == "void setOnTouchListener(android.view.View$OnTouchListener)") {
            return setEventListener(d, context, invokeExpr.asInstanceOf[InstanceInvokeExpr], stmt, "boolean onTouch(android.view.View,android.view.MotionEvent)", EventType.touch)
          }
          if (invokeExpr.getMethod.getSignature == "<android.view.View: void setId(int)>") {
            val dialogBase = invokeExpr.asInstanceOf[InstanceInvokeExpr].getBase.asInstanceOf[Local]
            invokeExpr.getArg(0) match {
              case intConstant: IntConstant => return d.setId(context.getMethod, dialogBase, stmt, intConstant.value)
              case _ =>
            }
          }
          if (invokeExpr.getMethod.getName == "setLayoutParams") {
            if (invokeExpr.getMethod.getSignature == "<android.view.View: void setLayoutParams(android.view.ViewGroup$LayoutParams)>") {
              val viewBase = invokeExpr.asInstanceOf[InstanceInvokeExpr].getBase.asInstanceOf[Local]
              val paramsLocal = invokeExpr.getArg(0).asInstanceOf[Local]
              return d.setLayoutParams(context.getMethod, stmt, viewBase, paramsLocal)
            }
            println("Unhandled: " + invokeExpr.getMethod.getSignature)
          }
          if (invokeExpr.getMethod.getName == "setTitle") {
            if(Constants.isDialogSetTitle(invokeExpr.getMethod.getSignature)) {
              val viewBase = invokeExpr.asInstanceOf[InstanceInvokeExpr].getBase.asInstanceOf[Local]
              val param = invokeExpr.getArg(0)
              return d.setDialogTitle(context.getMethod, stmt, viewBase, param)
            }
          }
          if (invokeExpr.getMethod.getName == "setMessage") {
            if(Constants.isDialogSetMessage(invokeExpr.getMethod.getSignature)) {
              val viewBase = invokeExpr.asInstanceOf[InstanceInvokeExpr].getBase.asInstanceOf[Local]
              val param = invokeExpr.getArg(0)
              return d.setDialogMessage(context.getMethod, stmt, viewBase, param)
            }
          }
          // FIXME: improve precision, handle View argument
          // FIMXE: sub-class won't get the correct predicat here
          if (Constants.isActivitySetContentViewWithInt(invokeExpr.getMethod)) {
            invokeExpr.getArg(0) match {
              case intConstant: IntConstant => return setContentView(context, d, invokeExpr, stmt, Left(intConstant.value))
              case _ =>
            }
          }
          if (Constants.isActivitySetContentViewWithView(invokeExpr.getMethod)) {
            return setContentView(context, d, invokeExpr, stmt, Right(invokeExpr.getArg(0).asInstanceOf[Local]))
          }
          if (Constants.isDialogSetContentViewWithInt(invokeExpr.getMethod)) {
            val dialogBase = invokeExpr.asInstanceOf[InstanceInvokeExpr].getBase.asInstanceOf[Local]
            invokeExpr.getArg(0) match {
              case intConstant: IntConstant =>
                return setContentViewDialog(d, context.getMethod, stmt, dialogBase, intConstant.value)
              case _ =>
            }
          }
          if (Constants.isDialogSetContentViewWithView(invokeExpr.getMethod)) {
            val dialogBase = invokeExpr.asInstanceOf[InstanceInvokeExpr].getBase.asInstanceOf[Local]
            return d.setContentViewDialog(context.getMethod, stmt, dialogBase, invokeExpr.getArg(0).asInstanceOf[Local])
          }
          if (AppInfo.hier.isSubclassOf(invokeExpr.getMethod.getDeclaringClass, Constants.layoutParamsClass)) {
            if (invokeExpr.getMethod.getName == "<init>") {
              val paramsBase = invokeExpr.asInstanceOf[InstanceInvokeExpr].getBase.asInstanceOf[Local]
              if (invokeExpr.getMethod.getSubSignature == "void <init>(int,int)") {
                return d.initLayoutParams(context.getMethod, stmt, paramsBase, invokeExpr.getArg(0), invokeExpr.getArg(1))
              }
              println("Unhandled: " + invokeExpr.getMethod.getSignature)
            }
          }
          invokeExpr match {
            case instanceInvokeExpr: InstanceInvokeExpr =>
              val sootClass = instanceInvokeExpr.getMethod.getDeclaringClass
              if (UnifiedObjectAPI.isBaseClass(sootClass)) {
                return d.updateUnifiedObjectNodes(context.getMethod, stmt, instanceInvokeExpr)
              }
            case _ =>
          }
        }
        d
    }
  }

  private def setContentView(context: DomainContext, d: AFTDomain, invokeExpr: InvokeExpr,
                             stmt: Stmt, viewArg: Either[Int, Local]): AFTDomain = {
    val nodes = d.getNodes(context.getMethod, stmt, invokeExpr.asInstanceOf[InstanceInvokeExpr].getBase.asInstanceOf[Local])
    var output = d
    for (node <- nodes) {
      node match {
        case ActNode(sootClass) =>
          viewArg match {
            case Left(id) => output = setContentViewAct(context, output, stmt, sootClass, id)
            case Right(viewLocal) => output = output.setContentViewAct(context.getMethod, stmt, sootClass, viewLocal)
          }
        case _ => // TODO: WebView etc.
      }
    }
    output
  }

  override def boundaryValue(m: SootMethod): Domain = topValue()

  override def copy(d: Domain): Domain = d

  override def meet(d1: Domain, d2: Domain): Domain = d1.meet(d2)

  private val aftProgramRepresentation = new AFTProgramRepresentation(entryPoints)
  override def programRepresentation(): ProgramRepresentation[SootMethod, soot.Unit] = aftProgramRepresentation

  override def topValue(): Domain = AFTDomain.top
}
