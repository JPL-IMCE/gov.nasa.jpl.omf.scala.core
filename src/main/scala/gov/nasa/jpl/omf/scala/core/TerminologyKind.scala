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

import scala.{Boolean,Enumeration}

/**
 * TerminologyKind indicates whether the semantics of the vocabulary of type terms defined 
 * in a TerminologyGraph (TBox graph) is open-world (isDefinition) or closed-world (isDesignation)
 */
object TerminologyKind extends Enumeration {
  type TerminologyKind = Value

  /**
   * isDefinition indicates that the semantics of a TerminologyGraph (TBox graph) is open-world.
   *
   * For example, this means that the semantics of taxonomic relations (e.g., EntityConceptSubClassAxiom) 
   * and restrictions (e.g., EntityConceptRestrictionAxiom) is open world in the following sense: 
   * Suppose there is a Definition TBox graph G where a taxonomic relationship asserts 
   * that concepts B and C are subclasses of concept A. The open world semantics of G as a Definition TBox means 
   * that there may exist another concept distinct from B or C that is asserted to be subclass of A in a some
   * other Definition TBox graph that directly or indirectly imports G.
   */
  val isDefinition = Value

  val isToplevelDefinition = Value

  /**
   * isDesignation indicates that the semantics of a TerminologyGraph (TBox graph) is closed-world.
   *
   * For example, this means that the semantics of taxonomic relations (e.g., EntityConceptSubClassAxiom) 
   * and restrictions (e.g., EntityConceptRestrictionAxiom) is closed world in the following sense: 
   * Suppose there is a Designation TBox graph G where the only taxonomic relationships asserted about concept A is that
   * concepts B and C are subclasses of concept A. The closed world semantics of G as a Designation TBox means that
   * B and C designate distinct sets of things (this is the so-called "Unique Name Assumption") and 
   * that there does not exist any other concept distinct from B or C that can be a subclass of A anywhere else.
   */
  val isDesignation = Value

  val isToplevelDesignation = Value

  def isDefinitionKind( k: TerminologyKind ): Boolean =
  k match {
    case _ @ ( TerminologyKind.isDefinition | TerminologyKind.isToplevelDefinition ) => true
    case _ => false
  }

  def isDesignationKind( k: TerminologyKind ): Boolean =
    k match {
      case _ @ ( TerminologyKind.isDesignation | TerminologyKind.isToplevelDesignation ) => true
      case _ => false
    }

  /**
   * Asymmetric comparison
   *
   * @param childKind The TerminologyKind of a "child" graph (child means extending child or nested child)
   * @param parentKind The TerminologyKind of a "parent" graph (parent means extended parent or nesting parent)
   * @return
   */
  def compatibleKind
  ( childKind: TerminologyKind )
  ( parentKind: TerminologyKind )
  : Boolean =
    isDesignationKind(childKind) || isDefinitionKind(childKind) && isDefinitionKind(parentKind)


}