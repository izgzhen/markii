/*
 * Copyright (c) 2020. Zhen Zhang
 */

package com.research.nomad.markii

import org.apache.commons.text.StringEscapeUtils
import presto.android.gui.listener.EventType
import soot.SootClass
import soot.SootMethod
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Paths

import scala.collection.mutable

object FactsWriter {
  // FIXME (minor): Use sum type and reflection
  object Fact extends Enumeration {
    type Fact = Value
    val
      //----- View properties -----//
      idName,
      layoutHeight, layoutHeightDIP, layoutHeightSP,
      layoutWidth, layoutWidthDIP, layoutWidthSP,
      textSize, textSizeDIP, textSizeSP,
      background,
      viewClass,
      textContent,
      imageHasNoContentDescription,

      //----- Layout skeleton analysis -----//
      containsView,
      layout_constraintEnd_toStartOf, layout_constraintStart_toEndOf,
      layout_constraintEnd_toEndOf, layout_constraintStart_toStartOf,
      layout_constraintBottom_toTopOf, layout_constraintTop_toBottomOf,
      layout_constraintBottom_toBottomOf, layout_constraintTop_toTopOf,

      //----- Control-flow, life-cycle transition etc. -----//
      rootView, mainActivity,
      activityEventHandler,
      eventHandler,
      alertDialogFixedButtonHandler,
      finishActivity,
      showDialog, // TODO: it needs to replace activityHandlerAllocDialog
      dismiss,
      intentFilter,

      startActivity,
      startViewActivityOfSomeHosts,
      startViewActivityOfMarketHost,

      //----- Value properties and Behaviors -----//
      buttonView, lifecycleMethod, preferenceActivity, dialogView,
      setStatus, setConsetInfoUpdateHandler, setNPA, readAudio, loadGoogleConsentForm,
      recordButton, loadWebViewUrl, adViewClass, serviceClass, adViewIdName,
      showSuspiciousAd, showSuspiciousInterstitialAd, invokesReflectiveAPI, serviceClassLastName,
      downloadApp, actionButton,
      showAd, showInterstitialAd, // FIXME: factor into reachable + ad API set?

      // For custom state
      recorderTransition
      = Value
  }
}

/**
 * Write facts in the input format for Souffle solver (https://souffle-lang.github.io/simple)
 *
 * @param factDir: Directory for facts. Created on initialization.
 */
class FactsWriter(val factDir: String) {
  private val writers = mutable.Map[String, BufferedWriter]()
  private val written = mutable.Set[(String, String)]()
  private val nameCounter = mutable.Map[String, Int]()

  Files.createDirectories(Paths.get(factDir))
  for (constraint <- FactsWriter.Fact.values) {
    writers.put(constraint.toString, new BufferedWriter(new FileWriter(factDir + "/" + constraint.toString + ".facts")))
  }

  def getNameCounter: Map[String, Int] = nameCounter.toMap

  private val storedConstraints = mutable.Map[FactsWriter.Fact.Value, mutable.ArrayBuffer[List[Any]]]()

  def getStoredConstraints(constraint: FactsWriter.Fact.Value): Iterable[List[Any]] =
    storedConstraints.getOrElse(constraint, Iterable.empty)

  def writeConstraint(constraint: FactsWriter.Fact.Value, args: Any*): Unit = {
    storedConstraints.getOrElseUpdate(constraint, mutable.ArrayBuffer()).addOne(args.toList)
    writeConstraint_(constraint, args: _*)
  }

  private def writeConstraint_(constraint: FactsWriter.Fact.Value, args: Any*): Unit = {
    val builder = new StringBuilder
    var noTab = true
    for (arg <- args) {
      if (noTab) noTab = false
      else builder.append("\t")
      arg match {
        case _: Integer => builder.append(arg.toString)
        case method: SootMethod => builder.append(StringEscapeUtils.escapeHtml4(method.getSignature))
        case clazz: SootClass => builder.append(StringEscapeUtils.escapeHtml4(clazz.getName))
        case str: String => builder.append(StringEscapeUtils.escapeHtml4(str))
        case _: EventType => builder.append(arg)
        case _ => throw new RuntimeException("Write invalid arg " + arg + ": " + arg.getClass)
      }
    }
    builder.append("\n")
    val line = builder.toString
    val name = constraint.toString
    if (!written.contains((name, line))) {
      written.add((name, line))
      try getWriter(name).write(line)
      catch {
        case e: IOException =>
          e.printStackTrace()
          System.exit(-1)
      }
      if (!nameCounter.contains(name)) nameCounter.put(name, 0)
      nameCounter.put(name, nameCounter(name) + 1)
    }
  }

  def writeDimensionConstraint(constraint: FactsWriter.Fact.Value, property: String, id: Int): Unit = {
    if (property.endsWith("dip")) {
      val dipIndex = property.indexOf("dip")
      val x = property.substring(0, dipIndex).toFloat
      writeConstraint(FactsWriter.Fact.withName(constraint.toString + "DIP"), x.round, id)
      return
    }
    if (property.endsWith("sp")) {
      val spIndex = property.indexOf("sp")
      val x = property.substring(0, spIndex).toFloat
      writeConstraint(FactsWriter.Fact.withName(constraint.toString + "SP"), x.round, id)
      return
    }
    writeConstraint(constraint, property, id)
  }

  @throws[IOException]
  private def getWriter(name: String): BufferedWriter = {
    if (!writers.contains(name)) {
      writers.put(name, new BufferedWriter(new FileWriter(factDir + "/" + name + ".facts")))
    }
    writers(name)
  }

  @throws[IOException]
  def close(): Unit = {
    for (writer <- writers.values) {
      writer.close()
    }
  }
}
