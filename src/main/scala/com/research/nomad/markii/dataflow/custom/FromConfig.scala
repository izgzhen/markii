/*
 * Copyright (c) 2020. Zhen Zhang
 */

package com.research.nomad.markii.dataflow.custom

import java.io.FileReader

import com.research.nomad.markii.dataflow.{AbsValSet, CustomObjectStateTransformer, Ref}
import soot.{SootClass, SootMethod}
import soot.jimple.{InstanceInvokeExpr, Stmt}

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import cats.syntax.either._
import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto._
import io.circe.yaml

case class Transition(fromStates: Option[Set[String]], toState: String)
case class APISemanticConfig(baseClassName: String, states: List[String], initialState: String,
                             transferMap: Map[String, Transition])

class FromConfig(configPath: String) extends CustomObjectStateTransformer[AbsValSet[String]] {
  type D = AbsValSet[String]
  implicit val transitionDecoder : Decoder[Transition] = deriveDecoder[Transition];

  private val json = yaml.parser.parse(new FileReader(configPath))

  private val api = json
    .leftMap(err => err: Error)
    .flatMap(_.as[APISemanticConfig])
    .valueOr(throw _)

  override def initInstance(sootClass: SootClass): Option[D] = {
    print("JSON: ", json);
    if (sootClass.getName == api.baseClassName) {
      Some(AbsValSet(Set(api.initialState)))
    } else {
      None
    }
  }

  /**
   * Key: Statement
   * Value: Set of possible transitions (ref-id, prev-state, post-state)
   */
  private val transitionSites = mutable.Map[Stmt, mutable.Set[(Option[Ref], String, String)]]()

  def getTransitions(m: SootMethod): Set[(Option[Ref], String, String)] = {
      
  print("HERE!!", api.transferMap);
    m.getActiveBody.getUnits.asScala.flatMap(u => transitionSites.getOrElse(u.asInstanceOf[Stmt], Set())).toSet
 
  }

  override def updatedInstance(prevVals: D, instanceInvokeExpr: InstanceInvokeExpr, callSite: Stmt, ref: Option[Ref]): D = {
  print("HERE");
   api.transferMap.get(instanceInvokeExpr.getMethod.getSignature) match {
      case Some(transition) =>
        print("HERE: ", transition);
        // var inFromSet : Boolean = false;
        for (prevVal <- prevVals.vals) {
          if( transition.fromStates.isEmpty || transition.fromStates.contains(prevVal)){
            // inFromSet = true;
            transitionSites.getOrElseUpdate(callSite, mutable.Set()).add((ref, prevVal, transition.toState))
          }
        }
        // if(inFromSet)
         AbsValSet(Set(transition.toState))
        // else{
        //   None
        // }
      case None =>
        print("HERE: NONE" );
        prevVals
    }
  }
}
