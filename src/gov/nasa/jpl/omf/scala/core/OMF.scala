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
 * 9 concrete types of TBox terms: ModelTypeTerm
 * 
 * ModelEntityAspect
 * ModelEntityConcept
 * ModelEntityRelationship
 * ModelScalarDataType
 * ModelStructuredDataType
 * ModelDataRelationshipFromEntityToScalar
 * ModelDataRelationshipFromEntityToStructure
 * ModelDataRelationshipFromStructureToScalar
 * ModelDataRelationshipFromStructureToStructure
 * 
 * 5 concrete types of TBox axioms: ModelTermAxiom
 * 
 * EntityDefinitionAspectSubClassAxiom
 * EntityConceptSubClassAxiom
 * EntityConceptRestrictionAxiom
 * EntityRelationshipSubClassAxiom
 * ScalarDataTypeFacetRestriction
 * 
 * 1 concrete type of ABox: ModelInstanceGraph
 * 
 * 8 concrete types of ABox assertions: ModelInstanceAssertion
 * 
 * ModelInstanceObject
 * ModelInstanceRelation
 * ModelInstanceDataLiteral
 * ModelInstanceDataStructure
 * ModelInstanceDataRelationshipFromEntityToScalar
 * ModelInstanceDataRelationshipFromEntityToStructure
 * ModelInstanceDataRelationshipFromStructureToScalar
 * ModelInstanceDataRelationshipFromStructureToStructure
 */
trait OMF extends OMFstore with OMFiri with OMFtbox with OMFabox
  
trait OMFstore {
  /**
   * The type of an OMF storage provider. An instance of a Store is an implicit parameter for the construction-related operations in OMFDSL.
   */
  type Store
  
}
  
trait OMFiri {
  
  type IRI     
}

/**
 *  types for IMCE T-Box ontologies  
 */
trait OMFtbox {
  
  type ModelTerminologyGraph
  type ImmutableModelTerminologyGraph <: ModelTerminologyGraph
  type MutableModelTerminologyGraph <: ModelTerminologyGraph
  
  type ModelTypeTerm
  
  type ModelEntityDefinition <: ModelTypeTerm
  type ModelEntityAspect <: ModelEntityDefinition  
  type ModelEntityConcept <: ModelEntityDefinition
  type ModelEntityRelationship <: ModelEntityDefinition
    
  type ModelDataTypeDefinition <: ModelTypeTerm
  type ModelScalarDataType <: ModelDataTypeDefinition
  type ModelStructuredDataType <: ModelDataTypeDefinition
    
  type ModelDataRelationship <: ModelTypeTerm
  
  type ModelDataRelationshipFrom
  type ModelDataRelationshipFromEntity <: ModelDataRelationshipFrom
  type ModelDataRelationshipFromStructure <: ModelDataRelationshipFrom
  
  type ModelDataRelationshipTo
  type ModelDataRelationshipToScalar <: ModelDataRelationshipTo
  type ModelDataRelationshipToStructure <: ModelDataRelationshipTo

  type ModelDataRelationshipFromEntityToScalar <: ModelDataRelationship with ModelDataRelationshipFromEntity with ModelDataRelationshipToScalar
  type ModelDataRelationshipFromEntityToStructure <: ModelDataRelationship with ModelDataRelationshipFromEntity with ModelDataRelationshipToStructure

  type ModelDataRelationshipFromStructureToScalar <: ModelDataRelationship with ModelDataRelationshipFromStructure with ModelDataRelationshipToScalar
  type ModelDataRelationshipFromStructureToStructure <: ModelDataRelationship with ModelDataRelationshipFromStructure with ModelDataRelationshipToStructure  
  
  type ModelTermAxiom
  
  type EntityDefinitionAspectSubClassAxiom <: ModelTermAxiom  // (sub, sup)
  type EntityConceptSubClassAxiom <: ModelTermAxiom				  // (sub, sup)
  type EntityConceptRestrictionAxiom <: ModelTermAxiom			  // (sub, rel, range) = ObjectAllValuesFrom
  type EntityRelationshipSubClassAxiom <: ModelTermAxiom	    // (sub, sup)  
  type ScalarDataTypeFacetRestriction <: ModelTermAxiom      // (sup, sup, [facet, literal]+)
  
}

/**
 *  types for IMCE A-Box ontologies
 */
trait OMFabox {
  
  type ModelInstanceGraph
  type ImmutableModelInstanceGraph <: ModelInstanceGraph
  type MutableModelInstanceGraph <: ModelInstanceGraph
  
  type ModelInstanceAssertion
  
  type ModelNamedIndividual <: ModelInstanceAssertion
  
  type ModelEntityInstance <: ModelNamedIndividual
  type ModelInstanceObject <: ModelEntityInstance
  type ModelInstanceRelation <: ModelEntityInstance
  
  type ModelDataInstance
  type ModelInstanceDataLiteral <: ModelInstanceAssertion with ModelDataInstance
  type ModelInstanceDataStructure <: ModelNamedIndividual with ModelDataInstance
  
  type ModelInstanceDataRelationshipFromEntityToScalar <: ModelInstanceAssertion
  type ModelInstanceDataRelationshipFromEntityToStructure <: ModelInstanceAssertion
  type ModelInstanceDataRelationshipFromStructureToScalar <: ModelInstanceAssertion
  type ModelInstanceDataRelationshipFromStructureToStructure <: ModelInstanceAssertion
  
}