/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2015, California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * *   Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * *   Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * *   Neither the name of Caltech nor its operating division, the Jet
 *    Propulsion Laboratory, nor the names of its contributors may be
 *    used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package gov.nasa.jpl.omf.scala.core

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
   * Asymetric comparison
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