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
package gov.nasa.jpl.omf.scala.core

/**
 * toplevel types:
 * 
 * IRI
 * 
 * ModelTerminologyGraph
 * ModelTypeTerm
 * ModelTermAxiom
 * 
 * ModelInstanceGraph
 * ModelInstanceAssertion
 * 
 * concrete types:
 * 
 * 1 concrete type of TBox: ModelTerminologyGraph
 * 
 * 6 concrete types of TBox terms: ModelTypeTerm
 * 
 * ModelEntityConcept
 * ModelEntityRelationship
 * ModelScalarDataType
 * ModelStructuredDataType
 * ModelStructuredDataRelationship
 * ModelEntityDataRelationship
 * 
 * 4 concrete types of TBox axioms: ModelTermAxiom
 * 
 * EntityConceptSubClassAxiom
 * EntityConceptRestrictionAxiom
 * EntityRelationshipSubClassAxiom
 * ScalarDataTypeFacetRestriction
 * 
 * 1 concrete type of ABox: ModelInstanceGraph
 * 
 * 6 concrete types of ABox assertions: ModelInstanceAssertion
 * 
 * ModelInstanceObject
 * ModelInstanceRelation
 * ModelInstanceDataLiteral
 * ModelInstanceDataStructure
 * ModelStructuredDataProperty
 * ModelEntityDataProperty
 */
trait OMF {
  
  type IRI
    
  // types for IMCE T-Box ontologies
  
  type ModelTerminologyGraph
  
  type ModelTypeTerm
  
  type ModelEntityDefinition <: ModelTypeTerm
  type ModelEntityConcept <: ModelEntityDefinition
  type ModelEntityRelationship <: ModelEntityDefinition
    
  type ModelDataTypeDefinition <: ModelTypeTerm
  type ModelScalarDataType <: ModelDataTypeDefinition
  type ModelStructuredDataType <: ModelDataTypeDefinition
    
  type ModelStructuredDataRelationship <: ModelTypeTerm
  
  type ModelEntityDataRelationship <: ModelTypeTerm
  
  type ModelTermAxiom
  
  type EntityConceptSubClassAxiom <: ModelTermAxiom				// (sub, sup)
  type EntityConceptRestrictionAxiom <: ModelTermAxiom			// (sub, rel, range) = ObjectAllValuesFrom
  type EntityRelationshipSubClassAxiom <: ModelTermAxiom	  // (sub, sup)
  
  type ScalarDataTypeFacetRestriction <: ModelTermAxiom    // (sup, sup, [facet, literal]+)
  
  // types for IMCE A-Box ontologies
  
  type ModelInstanceGraph
  
  type ModelInstanceAssertion
  
  type ModelNamedIndividual <: ModelInstanceAssertion
  
  type ModelEntityInstance <: ModelNamedIndividual
  type ModelInstanceObject <: ModelEntityInstance
  type ModelInstanceRelation <: ModelEntityInstance
  
  type ModelDataInstance <: ModelInstanceAssertion
  type ModelInstanceDataLiteral <: ModelDataInstance
  type ModelInstanceDataStructure <: ModelNamedIndividual with ModelDataInstance
  
  type ModelInstanceProperty <: ModelInstanceAssertion
  
  type ModelStructuredDataProperty <: ModelInstanceProperty
  
  type ModelEntityDataProperty <: ModelInstanceProperty

}