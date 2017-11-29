/*
 * Copyright 2015 California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * License Terms
 */

package gov.nasa.jpl.omf.scala.core

import gov.nasa.jpl.imce.oml.resolver

import gov.nasa.jpl.omf.scala.core.OMFError.Throwables

import scala.collection.immutable._
import scala.{Boolean, Int, Option, None, Some, StringContext}
import scala.Predef.ArrowAssoc
import scalaz._
import Scalaz._

case class Mutable2ImmutableModuleTable[omf <: OMF]
( pairs
  : Seq[(omf#MutableModule, omf#ImmutableModule)]
  = Seq.empty[(omf#MutableModule, omf#ImmutableModule)],
  ms
  : Map[resolver.api.taggedTypes.ModuleUUID, omf#MutableModule]
  = Map.empty[resolver.api.taggedTypes.ModuleUUID, omf#MutableModule],
  is
  : Map[resolver.api.taggedTypes.ModuleUUID, omf#ImmutableModule]
  = Map.empty[resolver.api.taggedTypes.ModuleUUID, omf#ImmutableModule]) {

  def keys: Seq[omf#MutableModule] = pairs.map(_._1)

  def values: Seq[omf#ImmutableModule] = pairs.map(_._2)

  def terminologyBoxValues
  (implicit store: omf#Store, ops: OMFOps[omf])
  : Seq[omf#ImmutableTerminologyBox]
  = pairs.map(_._2) flatMap ops.toImmutableTerminologyBox

  def terminologyGraphValues
  (implicit store: omf#Store, ops: OMFOps[omf])
  : Seq[omf#ImmutableTerminologyGraph]
  = pairs.map(_._2) flatMap ops.toImmutableTerminologyGraph

  def bundleValues
  (implicit store: omf#Store, ops: OMFOps[omf])
  : Seq[omf#ImmutableBundle]
  = pairs.map(_._2) flatMap ops.toImmutableBundle

  def descriptionBoxValues
  (implicit store: omf#Store, ops: OMFOps[omf])
  : Seq[omf#ImmutableDescriptionBox]
  = pairs.map(_._2) flatMap ops.toImmutableDescriptionBox

  def containsKey
  (mm: omf#MutableModule)
  (implicit ops: OMFOps[omf])
  : Boolean
  = ms.contains(ops.getModuleUUID(mm))

  def lookupKey
  (iri: omf#IRI)
  (implicit ops: OMFOps[omf])
  : Option[omf#MutableModule]
  = keys.find(ops.getModuleIRI(_) == iri)

  def containsValue
  (im: omf#ImmutableModule)
  (implicit ops: OMFOps[omf])
  : Boolean
  = is.contains(ops.getModuleUUID(im))

  def lookupValue
  (iri: omf#IRI)
  (implicit ops: OMFOps[omf])
  : Option[omf#ImmutableModule]
  = values.find(ops.getModuleIRI(_) == iri)

  def getImmutableModule
  (iri: omf#IRI)
  (implicit store: omf#Store, ops: OMFOps[omf])
  : Throwables \/ omf#ImmutableModule
  = values.find(ops.getModuleIRI(_) == iri) match {
    case Some(i) =>
      i.right
    case _ =>
      Set[java.lang.Throwable](OMFError.omfError(
        s"getImmutableModule: $iri not found"
      )).left
  }

  def getImmutableTerminologyBox
  (iri: omf#IRI)
  (implicit store: omf#Store, ops: OMFOps[omf])
  : Throwables \/ omf#ImmutableTerminologyBox
  = terminologyBoxValues
    .find(ops.getModuleIRI(_) == iri) match {
    case Some(i) =>
      i.right
    case _ =>
      Set[java.lang.Throwable](OMFError.omfError(
        s"getImmutableTerminologyBox: $iri not found"
      )).left
  }

  def getImmutableTerminologyGraph
  (iri: omf#IRI)
  (implicit store: omf#Store, ops: OMFOps[omf])
  : Throwables \/ omf#ImmutableTerminologyGraph
  = terminologyGraphValues
    .find(ops.getModuleIRI(_) == iri) match {
    case Some(i) =>
      i.right
    case _ =>
      Set[java.lang.Throwable](OMFError.omfError(
        s"getImmutableTerminologyGraph: $iri not found"
      )).left
  }

  def getImmutableBundle
  (iri: omf#IRI)
  (implicit store: omf#Store, ops: OMFOps[omf])
  : Throwables \/ omf#ImmutableBundle
  = bundleValues
    .find(ops.getModuleIRI(_) == iri) match {
    case Some(i) =>
      i.right
    case _ =>
      Set[java.lang.Throwable](OMFError.omfError(
        s"getImmutableBundle: $iri not found"
      )).left
  }

  def getImmutableDescriptionBox
  (iri: omf#IRI)
  (implicit store: omf#Store, ops: OMFOps[omf])
  : Throwables \/ omf#ImmutableDescriptionBox
  = descriptionBoxValues
    .find(ops.getModuleIRI(_) == iri) match {
    case Some(i) =>
      i.right
    case _ =>
      Set[java.lang.Throwable](OMFError.omfError(
        s"getImmutableDescriptionBox: $iri not found"
      )).left
  }

  def size: Int = pairs.size

  def `:+`
  (pair: (omf#MutableModule, omf#ImmutableModule))
  (implicit ops: OMFOps[omf])
  : Throwables \/ Mutable2ImmutableModuleTable[omf]
  = pairs.find(_._1 == pair._1) match {
    case Some((_, im)) =>
      if (im == pair._2)
        this.right
      else
        Set[java.lang.Throwable](
          OMFError.omfError(
            s"Mutable2ImmutableModuleTable: key conflict: ${pair._1} is already mapped to a different value"
          )).left
    case None =>
      val uuid = ops.getModuleUUID(pair._1)
      copy(pairs = pairs :+ pair, ms = ms + (uuid -> pair._1), is = is + (uuid -> pair._2)).right
  }

  def get(m: omf#MutableModule): Option[omf#ImmutableModule] = pairs.find(_._1 == m).map(_._2)

  def getImmutableTerminologyBox
  (m: omf#MutableModule)
  (implicit store: omf#Store, ops: OMFOps[omf])
  : Option[omf#ImmutableTerminologyBox]
  = get(m) flatMap ops.toImmutableTerminologyBox

  def getImmutableTerminologyGraph
  (m: omf#MutableModule)
  (implicit store: omf#Store, ops: OMFOps[omf])
  : Option[omf#ImmutableTerminologyGraph]
  = get(m) flatMap ops.toImmutableTerminologyGraph

  def getImmutableBundle
  (m: omf#MutableModule)
  (implicit store: omf#Store, ops: OMFOps[omf])
  : Option[omf#ImmutableBundle]
  = get(m) flatMap ops.toImmutableBundle

  def getImmutableDescriptionBox
  (m: omf#MutableModule)
  (implicit store: omf#Store, ops: OMFOps[omf])
  : Option[omf#ImmutableDescriptionBox]
  = get(m) flatMap ops.toImmutableDescriptionBox
}

object Mutable2ImmutableModuleTable {

  def empty[omf <: OMF]
  : Mutable2ImmutableModuleTable[omf]
  = Mutable2ImmutableModuleTable[omf]()
}