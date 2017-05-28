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

import gov.nasa.jpl.imce.oml.tables.{AnnotationEntry, AnnotationProperty}

import scala.{Int,Ordering}
import scala.collection.immutable._
import scala.Predef.String

package object core {

  type ImmutableTerminologyBoxSignature[omf <: OMF] =
    TerminologyBoxSignature[omf, scala.collection.immutable.Set, scala.collection.immutable.Set]

  type MutableTerminologyBoxSignature[omf <: OMF] =
    TerminologyBoxSignature[omf, scala.collection.mutable.HashSet, scala.collection.mutable.HashSet]

  type ImmutableDescriptionBoxSignature[omf <: OMF] =
    DescriptionBoxSignature[omf, scala.collection.immutable.Set, scala.collection.immutable.Set]

  type MutableDescriptionBoxSignature[omf <: OMF] =
    DescriptionBoxSignature[omf, scala.collection.mutable.HashSet, scala.collection.mutable.HashSet]

  implicit def annotationPropertyOrdering
  : Ordering[AnnotationProperty]
  = new Ordering[AnnotationProperty] {

    def compare(x: AnnotationProperty, y: AnnotationProperty)
    : Int
    = x.uuid.compareTo(y.uuid)

  }

  implicit def annotationOrdering
  : Ordering[AnnotationEntry]
  = new Ordering[AnnotationEntry] {

    def compare(x: AnnotationEntry, y: AnnotationEntry)
    : Int
    = x.moduleUUID.compareTo(y.moduleUUID) match {
      case c if 0 != c =>
        c
      case 0 =>
        x.subjectUUID.compareTo(y.subjectUUID) match {
          case c if 0 != c =>
            c
          case 0 =>
            x.value.compareTo(y.value)
        }
    }

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

  private val uuidG = gov.nasa.jpl.imce.oml.uuid.JVMUUIDGenerator()
  /**
    * Version 5 UUID based on a URL name in the standard URL namespace.
    *
    * @param namespace The prefix for the namespace.
    * @param factors key/value pairs that will be added to prefix to form the complete namespace
    * @return Version 5 UUID encoding of the namespace constructed from the prefix and key/value pairs
    */
  def generateUUID(namespace: String, factors: (String,_ <: String)*)
  : UUID
  = uuidG.namespaceUUID(namespace, factors : _*)

  def generateUUID(parentUUID: UUID, factors: (String,_ <: String)*)
  : UUID
  = generateUUID(parentUUID.toString, factors : _*)

  def getImportedModules[omf <: OMF]
  (m: omf#Module)
  ( implicit ops: OMFOps[omf], store: omf#Store )
  : Set[omf#Module]
  = ops.foldModule[Set[omf#Module]](
    funImmutableTerminologyGraph =
      (ig: omf#ImmutableTerminologyGraph) =>
        ops.immutableTerminologyGraphSignature(ig).importedModules,
    funMutableTerminologyGraph =
      (ig: omf#MutableTerminologyGraph) =>
        ops.mutableTerminologyGraphSignature(ig).importedModules,
    funImmutableTerminologyBundle =
      (ib: omf#ImmutableBundle) =>
        ops.immutableBundleSignature(ib).importedModules,
    funMutableTerminologyBundle =
      (ib: omf#MutableBundle) =>
        ops.mutableBundleSignature(ib).importedModules,
    funImmutableDescriptionBox =
      (id: omf#ImmutableDescriptionBox) =>
        ops.immutableDescriptionBoxSignature(id).importedModules,
    funMutableDescriptionBox =
      (id: omf#MutableDescriptionBox) =>
        ops.mutableDescriptionBoxSignature(id).importedModules
  )(m)

  def moduleImportClosure[Omf <: OMF, TG <: Omf#Module]
  ( g: TG )
  ( implicit ops: OMFOps[Omf], store: Omf#Store )
  : Set[Omf#Module] = {

    def step
    (gi: Omf#Module)
    : Set[Omf#Module]
    = getImportedModules(gi)

    val result
    : Set[Omf#Module]
    = OMFOps.closure[Omf#Module, Omf#Module](g, step) + g

    result
  }

}