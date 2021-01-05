package com.research.nomad.markii

import com.research.nomad.markii.dataflow.{CustomDomain, CustomStatePropVASCO}
import com.research.nomad.markii.dataflow.custom.{AbsInt, AbsIntVal}
import junit.framework.TestCase
import org.junit.Assert._
import org.junit.Test
import presto.android.Configs
import soot.Scene
import vasco.Helper

import scala.jdk.CollectionConverters._

//object TestAnalysis1 extends IAnalysis {
//  override def run(): Unit = {
//    Scene.v.getSootClass("com.example.Main3Activity")
//    val entrypoints = List(
//      Scene.v.getMethod("<com.example.test01.Main3Activity: void onCreate(android.os.Bundle)>")
//    )
//    val vascoProp = new CustomStatePropVASCO[AbsIntVal](entrypoints, AbsInt)
//    println("VASCO starts")
//    vascoProp.doAnalysis()
//    println("VASCO finishes")
//
//    val customVascoSolution = Helper.getMeetOverValidPathsSolution(vascoProp)
//
//    Util.dumpVASCOAbstractions[CustomDomain[AbsIntVal]](
//      "src/test/resources/testBasic/vasco-abstractions.txt",
//      customVascoSolution,
//      x => x.nonEmpty,
//      x => x.toJSONObj.toString,
//      entrypoints)
//  }
//}

/* Created at 5/29/20 by zhen */
class TestAnalysis extends TestCase {
//  TODO: Fix this
//  @Test def testBasic(): Unit = {
//    val androidSdk = sys.env("ANDROID_SDK")
//    val androidPlatforms =androidSdk + "/platforms"
//    val androidJar = androidPlatforms + "/android-29/android.jar"
//    Configs.customAnalysis = TestAnalysis1
//    Configs.runCustomAnalysis = true
//    val args = List(
//      "-configDir", "config",
//      "-sdkDir", androidSdk,
//      "-listenerSpecFile", "config/listeners.xml",
//      "-wtgSpecFile", "config/wtg.xml",
//      "-resourcePath", "src/test/resources/testBasic/res",
//      "-manifestFile", "src/test/resources/testBasic/AndroidManifest.xml",
//      "-project", "src/test/resources/testBasic/app-debug.apk",
//      "-apiLevel", "android-29",
//      "-benchmarkName", "app-debug.apk",
//      "-libraryPackageListFile", "config/libPackages.txt",
//      "-android", androidJar,
//      "-enableStringPropertyAnalysis",
//    )
//    presto.android.Main.main(args.toArray)
//  }

  @Test def testConstants(): Unit = {
    assert(Constants.isDialogBuilderShow("<android.app.AlertDialog$Builder: android.app.AlertDialog show()>"))
  }
}
