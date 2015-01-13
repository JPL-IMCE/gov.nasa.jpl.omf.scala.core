/*
 *
 *  License Terms
 *
 *  Copyright (c) 2015, California Institute of Technology ("Caltech").
 *  U.S. Government sponsorship acknowledged.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *
 *
 *   *   Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *   *   Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the
 *       distribution.
 *
 *   *   Neither the name of Caltech nor its operating division, the Jet
 *       Propulsion Laboratory, nor the names of its contributors may be
 *       used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 *  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package gov.nasa.jpl.omf.scala.core.syntax

import gov.nasa.jpl.omf.scala.core._

import scala.language.implicitConversions
import scala.language.postfixOps
import scala.util.Try
import scala.util.Failure
import scala.util.Success

trait OMFTBoxCatalogSyntax[omf <: OMF] { self: OMFSyntax[omf] =>
  
  def catalog(implicit ops: OMFOps[omf]) = OMFTBoxCatalog[omf](Map(), ops)

  implicit def tgraphW(tgraph: omf#ModelTerminologyGraph) = new TGraphW[omf](tgraph)

}

case class OMFTBoxCatalog[omf <: OMF](val tgraphs: Map[omf#IRI, omf#ModelTerminologyGraph], val ops: OMFOps[omf]) {

  val allTerminologyGraphs: Set[omf#ModelTerminologyGraph] = tgraphs.values toSet
  
  val allTypeTerms: Set[omf#ModelTypeTerm] = allTerminologyGraphs flatMap (ops.getTerms(_)._2) toSet
    
  val allIRIs: Set[omf#IRI] = allTypeTerms map (ops.fromTerm(_)) toSet
    
  val allEntityConcepts: Set[omf#ModelEntityConcept] = allTerminologyGraphs flatMap (ops.fromTerminologyGraph(_)._2) toSet
  val allEntityRelationships: Set[omf#ModelEntityRelationship] = allTerminologyGraphs flatMap (ops.fromTerminologyGraph(_)._3) toSet
  val allScalarDataTypes: Set[omf#ModelScalarDataType] = allTerminologyGraphs flatMap (ops.fromTerminologyGraph(_)._4) toSet
  val allStructuredDataTypes: Set[omf#ModelStructuredDataType] = allTerminologyGraphs flatMap (ops.fromTerminologyGraph(_)._5) toSet
  val allStructuredDataRelationships: Set[omf#ModelStructuredDataRelationship] = allTerminologyGraphs flatMap (ops.fromTerminologyGraph(_)._6) toSet
  val allEntityDataRelationships: Set[omf#ModelEntityDataRelationship] = allTerminologyGraphs flatMap (ops.fromTerminologyGraph(_)._7) toSet
  
  def addTBox(tbox: omf#ModelTerminologyGraph): Try[OMFTBoxCatalog[omf]] = {
    val ( iri, cs, rs, scs, sts, sdrs, edrs, ax ) = ops.fromTerminologyGraph( tbox )
    
    def makeFailure[T <: omf#ModelTypeTerm]( shouldBeEmpty: Set[T], message: String ): Try[OMFTBoxCatalog[omf]] =
      Failure(new IllegalArgumentException(s"${message}: ${shouldBeEmpty.map (ops.fromTerm(_)) mkString(", ")}"))
      
    val iriConflicts = allIRIs & (ops.getTerms(tbox)._2 map (ops.fromTerm(_)) toSet)
    if (iriConflicts.nonEmpty)
      return Failure(new IllegalArgumentException(s"conflicts with IRIs of terms already declared in the TBox catalog: ${iriConflicts mkString(", ")}"))
       
    val csConflicts = allEntityConcepts & cs.toSet
    if (csConflicts.nonEmpty)
      return makeFailure(csConflicts, "conflicts with EntityConcepts already declared in the TBox catalog")
    
    val rsConflicts = allEntityRelationships & rs.toSet
    if (rsConflicts.nonEmpty)
      return makeFailure(rsConflicts, "conflicts with EntityRelationships already declared in the TBox catalog")
    
    val scsConflicts = allScalarDataTypes & scs.toSet
    if (scsConflicts.nonEmpty)
      return makeFailure(scsConflicts, "conflicts with ScalarDataTypes already declared in the TBox catalog")
    
    val stsConflicts = allStructuredDataTypes & sts.toSet
    if (stsConflicts.nonEmpty)
      return makeFailure(stsConflicts, "conflicts with StructuredDataTypes already declared in the TBox catalog")
    
    val sdrsConflicts = allStructuredDataRelationships & sdrs.toSet
    if (sdrsConflicts.nonEmpty)
      return makeFailure(sdrsConflicts, "conflicts with StructuredDataRelationships already declared in the TBox catalog")
    
    val edrsConflicts = allEntityDataRelationships & edrs.toSet
    if (edrsConflicts.nonEmpty)
      return makeFailure(edrsConflicts, "conflicts with EntityDataRelationships already declared in the TBox catalog")
        
    val unionOfEntityDefinitions = allEntityConcepts ++ cs ++ allEntityRelationships ++ rs
    val unionOfEntityTypes = unionOfEntityDefinitions 
    
    val rsSources = rs map (ops.fromEntityRelationship(_)._2) toSet
    val unresolvedEntityRelationshipSources = rsSources -- unionOfEntityDefinitions
    if (unresolvedEntityRelationshipSources.nonEmpty)
      return makeFailure(unresolvedEntityRelationshipSources, "unresolved EntityRelationship sources")
    
    val rsTargets = rs map (ops.fromEntityRelationship(_)._3) toSet
    val unresolvedEntityRelationshipTargets = rsTargets -- unionOfEntityTypes
    if (unresolvedEntityRelationshipTargets.nonEmpty)
      return makeFailure(unresolvedEntityRelationshipTargets, "unresolved EntityRelationship targets")
     
    val unionOfStructuredDataTypes = allStructuredDataTypes ++ sts
    val unionOfDataTypeDefinitions = allScalarDataTypes ++ scs ++ unionOfStructuredDataTypes
    val unionOfDataTypes = unionOfDataTypeDefinitions
    
    val sdrsSources = sdrs map (ops.fromStructuredDataRelationship(_)._2) toSet
    val unresolvedStructuredDataRelationshipSources = sdrsSources -- unionOfStructuredDataTypes
    if (unresolvedStructuredDataRelationshipSources.nonEmpty)
      return makeFailure(unresolvedStructuredDataRelationshipSources, "unresolved StructuredDataRelationship sources")
      
    val sdrsTargets = sdrs map (ops.fromStructuredDataRelationship(_)._3) toSet
    val unresolvedStructuredDataRelationshipTargets = sdrsTargets -- unionOfDataTypes
    if (unresolvedStructuredDataRelationshipTargets.nonEmpty)
      return makeFailure(unresolvedStructuredDataRelationshipTargets, "unresolved StructuredDataRelationship targets")
      
    val edrsSources = edrs map (ops.fromEntityDataRelationship(_)._2) toSet
    val unresolvedEntityDataRelationshipSources = edrsSources -- unionOfEntityDefinitions
    if (unresolvedEntityDataRelationshipSources.nonEmpty)
      return makeFailure(unresolvedEntityDataRelationshipSources, "unresolved EntityDataRelationship sources")
      
    val edrsTargets = edrs map (ops.fromEntityDataRelationship(_)._3) toSet
    val unresolvedEntityDataRelationshipTargets = edrsTargets -- unionOfDataTypes
    if (unresolvedEntityDataRelationshipTargets.nonEmpty)
      return makeFailure(unresolvedEntityDataRelationshipTargets, "unresolved EntityDataRelationship targets")
      
    Success(OMFTBoxCatalog[omf]( tgraphs + ( iri -> tbox ), ops))
  }

  def getTBox( tns: omf#IRI ): Option[omf#ModelTerminologyGraph] =
    tgraphs get tns

}

class TGraphW[omf <: OMF](val tgraph: omf#ModelTerminologyGraph) extends AnyVal {

  def isIsomorphicWith(other: omf#ModelTerminologyGraph)(implicit ops: OMFOps[omf]): Boolean =
    ops.tboxIsomorphism(tgraph, other)

  def containsEntityConcept(concept: omf#ModelEntityConcept)(implicit ops: OMFOps[omf]): Boolean =
    ops.lookupEntityConcept(tgraph, ops.fromEntityConcept(concept)) match {
      case None    => false
      case Some(_) => true
    }

  def containsEntityRelationship(relationship: omf#ModelEntityRelationship)(implicit ops: OMFOps[omf]): Boolean =
    ops.lookupEntityRelationship(tgraph, ops.fromEntityRelationship(relationship)._1) match {
      case None    => false
      case Some(_) => true
    }

  def containsScalarDataType(sd: omf#ModelScalarDataType)(implicit ops: OMFOps[omf]): Boolean =
    ops.lookupScalarDataType(tgraph, ops.fromScalarDataType(sd)) match {
      case None    => false
      case Some(_) => true
    }

  def containsStructuredDataType(sd: omf#ModelStructuredDataType)(implicit ops: OMFOps[omf]): Boolean =
    ops.lookupStructuredDataType(tgraph, ops.fromStructuredDataType(sd)) match {
      case None    => false
      case Some(_) => true
    }

  def containsStructuredDataRelationship(relationship: omf#ModelStructuredDataRelationship)(implicit ops: OMFOps[omf]): Boolean =
    ops.lookupStructuredDataRelationship(tgraph, ops.fromStructuredDataRelationship(relationship)._1) match {
      case None    => false
      case Some(_) => true
    }

  def containsEntityDataRelationship(relationship: omf#ModelEntityDataRelationship)(implicit ops: OMFOps[omf]): Boolean =
    ops.lookupEntityDataRelationship(tgraph, ops.fromEntityDataRelationship(relationship)._1) match {
      case None    => false
      case Some(_) => true
    }
}