/*
 * Copyright (c) 2020. Zhen Zhang
 */

package com.research.nomad.markii

import com.research.nomad.markii.dataflow.AbsNode.ViewNode
import presto.android.Hierarchy
import presto.android.gui.IDNameExtractor
import presto.android.xml.{AndroidView, XMLParser}
import soot.{Scene, SootClass, SootMethod}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

object AppInfo {
  val hier: Hierarchy = Hierarchy.v()
  private val xmlParser: XMLParser = XMLParser.Factory.getXMLParser

  private val allHandlers = mutable.Set[SootMethod]()
  val allActivities: Set[SootClass] = hier.frameworkManaged.keySet().asScala.filter(c =>
    hier.applicationActivityClasses.contains(c) && !c.isAbstract).toSet
  val mainActivity: SootClass = xmlParser.getMainActivity

  def findViewById(id: Int): AndroidView = xmlParser.findViewById(id)

  private def initAllHandlers(): Unit = {
    for (c <- Scene.v().getApplicationClasses.asScala) {
      if (c.isConcrete && Constants.guiClasses.exists(listener => hier.isSubclassOf(c, listener))) {
        for (m <- c.getMethods.asScala) {
          if (m.hasActiveBody && m.getName.startsWith("on")) {
            allHandlers.add(m)
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
            allHandlers.add(m)
          }
        }
      }
    }
  }

  def init(): Unit = {
    initAllHandlers()
  }

  def getAllHandlers: Set[SootMethod] = allHandlers.toSet

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

  def getIntents: List[(String, SootMethod)] = {
    var ret = List[(String, SootMethod)]()
    for ((className, filters) <- xmlParser.getIntentFilters.asScala) {
      try {
        val curCls = Scene.v.getSootClass(className)
        if (curCls != null && curCls.isConcrete) {
          for (filter <- filters.asScala) {
            for (action <- filter.getActions.asScala) {
              val m = curCls.getMethodByNameUnsafe("onReceive")
              if (m != null) {
                ret :+= (action, m)
              }
            }
          }
        }
      } catch {
        case ignored: NullPointerException =>
      }
    }
    ret
  }

  def getServices: List[SootClass] = {
    var services = List[SootClass]()
    for (service <- xmlParser.getServices.asScala) {
      if (service != null) {
        val curCls = Scene.v.getSootClassUnsafe(service)
        if (curCls != null && curCls.isConcrete) {
          services :+= curCls
        }
      }
    }
    services
  }

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
}
