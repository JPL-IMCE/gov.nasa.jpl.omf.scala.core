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
import gov.nasa.jpl.omf.scala.core.builtin.BuiltInDatatypeMaps.DataRangeCategories

import scala.collection.immutable._
import scala.{Boolean, Int, None, Option, Some, StringContext}
import scalaz._
import Scalaz._

trait Mutable2ImmutableModuleTable[omf <: OMF[omf]] {

  protected val pairs: Seq[(omf#MutableModule, omf#ImmutableModule)]

  protected val ms: Map[resolver.api.taggedTypes.ModuleUUID, omf#MutableModule]

  protected val is: Map[resolver.api.taggedTypes.ModuleUUID, omf#ImmutableModule]

  protected val drc: DataRangeCategories[omf]

  def keys: Seq[omf#MutableModule] = pairs.map(_._1)

  def values: Seq[omf#ImmutableModule] = pairs.map(_._2)

  def modules: Iterable[omf#Module]
  = Iterable.empty[omf#Module] ++
    is.values ++
    ms.filter { case (uuid, _) => is.contains(uuid) }.values

  def terminologyBoxValues
  (implicit ops: OMFOps[omf])
  : Iterable[omf#TerminologyBox]
  = modules.flatMap(ops.foldTerminologyBox)

  def immutableTerminologyBoxValues
  (implicit ops: OMFOps[omf])
  : Iterable[omf#ImmutableTerminologyBox]
  = is.values.flatMap(ops.foldImmutableTerminologyBox).to[Iterable]

  def immutableTerminologyGraphValues
  (implicit ops: OMFOps[omf])
  : Iterable[omf#ImmutableTerminologyGraph]
  = is.values.flatMap(ops.foldImmutableTerminologyGraph).to[Iterable]

  def immutableBundleValues
  (implicit ops: OMFOps[omf])
  : Iterable[omf#ImmutableBundle]
  = is.values.flatMap(ops.foldImmutableBundle).to[Iterable]

  def immutableDescriptionBoxValues
  (implicit ops: OMFOps[omf])
  : Iterable[omf#ImmutableDescriptionBox]
  = is.values.flatMap(ops.foldImmutableDescriptionBox).to[Iterable]

  def terminologyGraphValues
  (implicit ops: OMFOps[omf])
  : Iterable[omf#TerminologyGraph]
  = modules.flatMap(ops.foldTerminologyGraph)

  def bundleValues
  (implicit ops: OMFOps[omf])
  : Iterable[omf#Bundle]
  = modules.flatMap(ops.foldBundle).to[Seq]

  def descriptionBoxValues
  (implicit ops: OMFOps[omf])
  : Iterable[omf#DescriptionBox]
  = modules.flatMap(ops.foldDescriptionBox)

  def containsKey
  (mm: omf#MutableModule)
  (implicit ops: OMFOps[omf])
  : Boolean
  = ms.contains(ops.getModuleUUID(mm))

  def lookupKey
  (iri: omf#IRI)
  (implicit ops: OMFOps[omf])
  : Option[omf#MutableModule]
  = ms.find { case (_, mm) => ops.getModuleIRI(mm) == iri }.map(_._2)

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

  def lookupImmutableModule
  (iri: omf#IRI)
  (implicit ops: OMFOps[omf])
  : Option[omf#ImmutableModule]
  = lookupValue(iri) orElse drc.lookupBuiltInModule(iri) flatMap
    ops.foldModule(
      funImmutableTerminologyGraph = Some.apply,
      funMutableTerminologyGraph = _ => None,
      funImmutableTerminologyBundle = Some.apply,
      funMutableTerminologyBundle = _ => None,
      funImmutableDescriptionBox = Some.apply,
      funMutableDescriptionBox = _ => None)

  def lookupMutableModule
  (iri: omf#IRI)
  (implicit ops: OMFOps[omf])
  : Option[omf#MutableModule]
  = lookupKey(iri) orElse drc.lookupBuiltInModule(iri) flatMap
    ops.foldModule(
      funImmutableTerminologyGraph = _ => None,
      funMutableTerminologyGraph = Some.apply,
      funImmutableTerminologyBundle = _ => None,
      funMutableTerminologyBundle = Some.apply,
      funImmutableDescriptionBox = _ => None,
      funMutableDescriptionBox = Some.apply)

  def lookupModule
  (iri: omf#IRI)
  (implicit ops: OMFOps[omf])
  : Option[omf#Module]
  = lookupImmutableModule(iri) orElse lookupMutableModule(iri)

  def getImmutableModule
  (iri: omf#IRI)
  (implicit ops: OMFOps[omf])
  : Throwables \/ omf#ImmutableModule
  = values.find(ops.getModuleIRI(_) == iri) match {
    case Some(i) =>
      i.right
    case _ =>
      Set[java.lang.Throwable](OMFError.omfError(
        s"getImmutableModule: $iri not found"
      )).left
  }

  def lookupImmutableTerminologyBox
  (iri: omf#IRI)
  (implicit ops: OMFOps[omf])
  : Option[omf#ImmutableTerminologyBox]
  = lookupValue(iri) orElse drc.lookupBuiltInModule(iri) flatMap
    ops.foldModule(
      funImmutableTerminologyGraph = Some.apply,
      funMutableTerminologyGraph = _ => None,
      funImmutableTerminologyBundle = Some.apply,
      funMutableTerminologyBundle = _ => None,
      funImmutableDescriptionBox = _ => None,
      funMutableDescriptionBox = _ => None)

  def lookupMutableTerminologyBox
  (iri: omf#IRI)
  (implicit ops: OMFOps[omf])
  : Option[omf#MutableTerminologyBox]
  = lookupKey(iri) orElse drc.lookupBuiltInModule(iri) flatMap
    ops.foldModule(
      funImmutableTerminologyGraph = _ => None,
      funMutableTerminologyGraph = Some.apply,
      funImmutableTerminologyBundle = _ => None,
      funMutableTerminologyBundle = Some.apply,
      funImmutableDescriptionBox = _ => None,
      funMutableDescriptionBox = _ => None)

  def lookupTerminologyBox
  (iri: omf#IRI)
  (implicit ops: OMFOps[omf])
  : Option[omf#TerminologyBox]
  = lookupImmutableTerminologyBox(iri) orElse lookupMutableTerminologyBox(iri)

  def lookupImmutableTerminologyGraph
  (iri: omf#IRI)
  (implicit ops: OMFOps[omf])
  : Option[omf#ImmutableTerminologyGraph]
  = lookupValue(iri) orElse drc.lookupBuiltInModule(iri) flatMap
    ops.foldModule(
      funImmutableTerminologyGraph = Some.apply,
      funMutableTerminologyGraph = _ => None,
      funImmutableTerminologyBundle = _ => None,
      funMutableTerminologyBundle = _ => None,
      funImmutableDescriptionBox = _ => None,
      funMutableDescriptionBox = _ => None)

  def lookupMutableTerminologyGraph
  (iri: omf#IRI)
  (implicit ops: OMFOps[omf])
  : Option[omf#MutableTerminologyGraph]
  = lookupKey(iri) orElse drc.lookupBuiltInModule(iri) flatMap
    ops.foldModule(
      funImmutableTerminologyGraph = _ => None,
      funMutableTerminologyGraph = Some.apply,
      funImmutableTerminologyBundle = _ => None,
      funMutableTerminologyBundle = _ => None,
      funImmutableDescriptionBox = _ => None,
      funMutableDescriptionBox = _ => None)

  def lookupTerminologyGraph
  (iri: omf#IRI)
  (implicit ops: OMFOps[omf])
  : Option[omf#TerminologyGraph]
  = lookupImmutableTerminologyGraph(iri) orElse lookupMutableTerminologyGraph(iri)

  def lookupImmutableBundle
  (iri: omf#IRI)
  (implicit ops: OMFOps[omf])
  : Option[omf#ImmutableBundle]
  = lookupValue(iri) orElse drc.lookupBuiltInModule(iri) flatMap
    ops.foldModule(
      funImmutableTerminologyGraph = _ => None,
      funMutableTerminologyGraph = _ => None,
      funImmutableTerminologyBundle = Some.apply,
      funMutableTerminologyBundle = _ => None,
      funImmutableDescriptionBox = _ => None,
      funMutableDescriptionBox = _ => None)

  def lookupMutableBundle
  (iri: omf#IRI)
  (implicit ops: OMFOps[omf])
  : Option[omf#MutableBundle]
  = lookupKey(iri) orElse drc.lookupBuiltInModule(iri) flatMap
    ops.foldModule(
      funImmutableTerminologyGraph = _ => None,
      funMutableTerminologyGraph = _ => None,
      funImmutableTerminologyBundle = _ => None,
      funMutableTerminologyBundle = Some.apply,
      funImmutableDescriptionBox = _ => None,
      funMutableDescriptionBox = _ => None)

  def lookupBundle
  (iri: omf#IRI)
  (implicit ops: OMFOps[omf])
  : Option[omf#Bundle]
  = lookupImmutableBundle(iri) orElse lookupMutableBundle(iri)

  def lookupImmutableDescriptionBox
  (iri: omf#IRI)
  (implicit ops: OMFOps[omf])
  : Option[omf#ImmutableDescriptionBox]
  = lookupValue(iri) orElse drc.lookupBuiltInModule(iri) flatMap
    ops.foldModule(
      funImmutableTerminologyGraph = _ => None,
      funMutableTerminologyGraph = _ => None,
      funImmutableTerminologyBundle = _ => None,
      funMutableTerminologyBundle = _ => None,
      funImmutableDescriptionBox = Some.apply,
      funMutableDescriptionBox = _ => None)

  def lookupMutableDescriptionBox
  (iri: omf#IRI)
  (implicit ops: OMFOps[omf])
  : Option[omf#MutableDescriptionBox]
  = lookupKey(iri) orElse drc.lookupBuiltInModule(iri) flatMap
    ops.foldModule(
      funImmutableTerminologyGraph = _ => None,
      funMutableTerminologyGraph = _ => None,
      funImmutableTerminologyBundle = _ => None,
      funMutableTerminologyBundle = _ => None,
      funImmutableDescriptionBox = _ => None,
      funMutableDescriptionBox = Some.apply)

  def lookupDescriptionBox
  (iri: omf#IRI)
  (implicit ops: OMFOps[omf])
  : Option[omf#DescriptionBox]
  = lookupImmutableDescriptionBox(iri) orElse lookupMutableDescriptionBox(iri)

  def getImmutableTerminologyBox
  (iri: omf#IRI)
  (implicit ops: OMFOps[omf])
  : Throwables \/ omf#ImmutableTerminologyBox
  = immutableTerminologyBoxValues
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
  (implicit ops: OMFOps[omf])
  : Throwables \/ omf#ImmutableTerminologyGraph
  = immutableTerminologyGraphValues
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
  (implicit ops: OMFOps[omf])
  : Throwables \/ omf#ImmutableBundle
  = immutableBundleValues
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
  (implicit ops: OMFOps[omf])
  : Throwables \/ omf#ImmutableDescriptionBox
  = immutableDescriptionBoxValues
    .find(ops.getModuleIRI(_) == iri) match {
    case Some(i) =>
      i.right
    case _ =>
      Set[java.lang.Throwable](OMFError.omfError(
        s"getImmutableDescriptionBox: $iri not found"
      )).left
  }

  def size(): Int = pairs.size

  def `:+`
  (pair: (omf#MutableModule, omf#ImmutableModule))
  (implicit ops: OMFOps[omf])
  : Throwables \/ Mutable2ImmutableModuleTable[omf]

  def get(m: omf#MutableModule): Option[omf#ImmutableModule] = pairs.find(_._1 == m).map(_._2)

}
