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

package gov.nasa.jpl.omf.scala

import java.util.UUID

import gov.nasa.jpl.imce.oml.tables.AnnotationProperty

import scala.{Int,Ordering,None,Some}
import scala.collection.immutable._
import scala.Predef.{ArrowAssoc,String}
import scalaz._

/**
  * Ontological Modeling Framework API
  */
package object core {

  type ImmutableTerminologyBoxSignature[omf <: OMF[omf]] =
    TerminologyBoxSignature[omf, scala.collection.immutable.Set]

  type MutableTerminologyBoxSignature[omf <: OMF[omf]] =
    TerminologyBoxSignature[omf, scala.collection.mutable.HashSet]

  type ImmutableDescriptionBoxSignature[omf <: OMF[omf]] =
    DescriptionBoxSignature[omf, scala.collection.immutable.Set]

  type MutableDescriptionBoxSignature[omf <: OMF[omf]] =
    DescriptionBoxSignature[omf, scala.collection.mutable.HashSet]

  implicit def annotationPropertyOrdering
  : Ordering[AnnotationProperty]
  = new Ordering[AnnotationProperty] {

    def compare(x: AnnotationProperty, y: AnnotationProperty)
    : Int
    = x.uuid.compareTo(y.uuid)

  }

  /**
    * Version 4 random UUID
    * @see https://en.wikipedia.org/wiki/Universally_unique_identifier#Version_4_.28random.29
    *
    * @return Version 4 random UUID
    */
  def generateUUID()
  : UUID
  = UUID.randomUUID()

  val uuidG = gov.nasa.jpl.imce.oml.uuid.JVMUUIDGenerator()
  /**
    * Version 5 UUID based on a URL name in the standard URL namespace.
    *
    * @param namespace The prefix for the namespace.
    * @param factors key/value pairs that will be added to prefix to form the complete namespace
    * @return Version 5 UUID encoding of the namespace constructed from the prefix and key/value pairs
    */
  def generateUUIDFromString(namespace: String, factors: (String,_ <: String)*)
  : UUID
  = uuidG.namespaceUUID(namespace, factors : _*)

  def generateUUIDFromUUID(parentUUID: String, factors: (String,_ <: UUID)*)
  : UUID
  = generateUUIDFromString(parentUUID, factors.map(f => f._1 -> f._2.toString) : _*)

  def generateUUIDFromString(parentUUID: UUID, factors: (String,_ <: String)*)
  : UUID
  = generateUUIDFromString(parentUUID.toString, factors : _*)

  def generateUUIDFromUUID(parentUUID: UUID, factors: (String,_ <: UUID)*)
  : UUID
  = generateUUIDFromString(parentUUID.toString, factors.map(f => f._1 -> f._2.toString) : _*)

  def importedTerminologies[omf <: OMF[omf]]
  (m: omf#Module)
  ( implicit ops: OMFOps[omf], store: omf#Store )
  : OMFError.Throwables \/ Set[omf#TerminologyBox]
  = ops.foldModule[OMFError.Throwables \/ Set[omf#TerminologyBox]](
    funImmutableTerminologyGraph =
      (ig: omf#ImmutableTerminologyGraph) =>
        ops.lookupTerminologyBoxes(ops.immutableTerminologyGraphSignature(ig).importedTerminologies),
    funMutableTerminologyGraph =
      (ig: omf#MutableTerminologyGraph) =>
        ops.lookupTerminologyBoxes(ops.mutableTerminologyGraphSignature(ig).importedTerminologies),
    funImmutableTerminologyBundle =
      (ib: omf#ImmutableBundle) =>
        ops.lookupTerminologyBoxes(ops.immutableBundleSignature(ib).importedTerminologies),
    funMutableTerminologyBundle =
      (ib: omf#MutableBundle) =>
        ops.lookupTerminologyBoxes(ops.mutableBundleSignature(ib).importedTerminologies),
    funImmutableDescriptionBox =
      (id: omf#ImmutableDescriptionBox) =>
        ops.lookupTerminologyBoxes(ops.immutableDescriptionBoxSignature(id).importedTerminologies),
    funMutableDescriptionBox =
      (id: omf#MutableDescriptionBox) =>
        ops.lookupTerminologyBoxes(ops.mutableDescriptionBoxSignature(id).importedTerminologies)
  )(m)

  def importedDescriptions[omf <: OMF[omf]]
  (m: omf#Module)
  ( implicit ops: OMFOps[omf], store: omf#Store )
  : OMFError.Throwables \/ Set[omf#DescriptionBox]
  = ops.foldModule[OMFError.Throwables \/ Set[omf#DescriptionBox]](
    funImmutableTerminologyGraph =
      (_: omf#ImmutableTerminologyGraph) =>
        \/-(Set.empty[omf#DescriptionBox]),
    funMutableTerminologyGraph =
      (_: omf#MutableTerminologyGraph) =>
        \/-(Set.empty[omf#DescriptionBox]),
    funImmutableTerminologyBundle =
      (_: omf#ImmutableBundle) =>
        \/-(Set.empty[omf#DescriptionBox]),
    funMutableTerminologyBundle =
      (_: omf#MutableBundle) =>
        \/-(Set.empty[omf#DescriptionBox]),
    funImmutableDescriptionBox =
      (id: omf#ImmutableDescriptionBox) =>
        ops.lookupDescriptionBoxes(ops.immutableDescriptionBoxSignature(id).importedDescriptions),
    funMutableDescriptionBox =
      (id: omf#MutableDescriptionBox) =>
        ops.lookupDescriptionBoxes(ops.mutableDescriptionBoxSignature(id).importedDescriptions)
  )(m)

  def computeTerminologyBoxImportClosure[omf <: OMF[omf]]
  ( m: omf#Module )
  ( implicit ops: OMFOps[omf], store: omf#Store )
  : Set[omf#TerminologyBox] = {

    def step
    (mi: omf#Module)
    : OMFError.Throwables \/ Set[omf#TerminologyBox]
    = importedTerminologies(mi)

    val result
    : Set[omf#TerminologyBox]
    = OMFOps.closureCheck[omf#Module, omf#TerminologyBox](m, step) match {
      case \/-(cls) =>
        cls
      case -\/(errors) =>
        throw errors.head
    }

    ops.toTerminologyBox(m) match {
      case Some(t) =>
        result + t
      case None =>
        result
    }
  }

  def terminologyBoxImportClosure[omf <: OMF[omf]]
  ( m: omf#Module )
  ( implicit ops: OMFOps[omf], store: omf#Store )
  : Set[omf#TerminologyBox]
  = ops.terminologyBoxImportClosure(m)

  def computeDescriptionBoxImportClosure[omf <: OMF[omf]]
  ( m: omf#Module )
  ( implicit ops: OMFOps[omf], store: omf#Store )
  : Set[omf#DescriptionBox] = {

    def step
    (mi: omf#Module)
    : OMFError.Throwables \/ Set[omf#DescriptionBox]
    = importedDescriptions(mi)

    val result
    : Set[omf#DescriptionBox]
    = OMFOps.closureCheck[omf#Module, omf#DescriptionBox](m, step) match {
      case \/-(cls) =>
        cls
      case -\/(errors) =>
        throw errors.head
    }

    ops.toDescriptionBox(m) match {
      case Some(d) =>
        result + d
      case None =>
        result
    }
  }

  def descriptionBoxImportClosure[omf <: OMF[omf]]
  ( m: omf#Module )
  ( implicit ops: OMFOps[omf], store: omf#Store )
  : Set[omf#DescriptionBox]
  = ops.descriptionBoxImportClosure(m)

}