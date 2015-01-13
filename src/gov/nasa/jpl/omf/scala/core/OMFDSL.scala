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

import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._

/**
 * @author Nicolas.F.Rouquette@jpl.nasa.gov
 */
trait OMFDSL[omf <: OMF] { this: OMFOps[omf] =>

  // IRI
  
  object IRI {
    def apply(s: String): omf#IRI = makeIRI(s)
    def unapply(iri: omf#IRI): Some[String] = Some(fromIRI(iri))
  }
  
  // terminology graph

  object ModelTerminologyGraph {

    def apply(iri: omf#IRI): omf#ModelTerminologyGraph =
      makeTerminologyGraph(iri, Nil, Nil, Nil, Nil, Nil, Nil, Nil)

    def apply(
      iri: omf#IRI,
      c: Iterable[omf#ModelEntityConcept],
      r: Iterable[omf#ModelEntityRelationship],
      sc: Iterable[omf#ModelScalarDataType],
      st: Iterable[omf#ModelStructuredDataType],
      sdr: Iterable[omf#ModelStructuredDataRelationship],
      edr: Iterable[omf#ModelEntityDataRelationship],
      ax: Iterable[omf#ModelTermAxiom]): omf#ModelTerminologyGraph =
      makeTerminologyGraph(iri, c, r, sc, st, sdr, edr, ax)

    def unapply(tg: omf#ModelTerminologyGraph): Some[(omf#IRI, Iterable[omf#ModelEntityConcept], Iterable[omf#ModelEntityRelationship], Iterable[omf#ModelScalarDataType], Iterable[omf#ModelStructuredDataType], Iterable[omf#ModelStructuredDataRelationship], Iterable[omf#ModelEntityDataRelationship], Iterable[omf#ModelTermAxiom])] =
      Some(fromTerminologyGraph(tg))
  }

  // entity concept
  
  object ModelEntityConcept {
    def apply(iri: omf#IRI): omf#ModelEntityConcept = makeEntityConcept(iri)
    def unapply(c: omf#ModelEntityConcept): Some[omf#IRI] = Some(fromEntityConcept(c))
  }

  def concept(s: String): omf#ModelEntityConcept = ModelEntityConcept(makeIRI(s))
  def concept(iri: omf#IRI): omf#ModelEntityConcept = ModelEntityConcept(iri)

  // entity relationship
  
  object ModelEntityRelationship {
    def apply(iri: omf#IRI, source: omf#ModelEntityDefinition, target: omf#ModelEntityDefinition, characteristics: RelationshipCharacteristics*): omf#ModelEntityRelationship = makeEntityRelationship(iri, source, target, characteristics.toIterable)
    def unapply(r: omf#ModelEntityRelationship): Some[(omf#IRI, omf#ModelEntityDefinition, omf#ModelEntityDefinition, Iterable[RelationshipCharacteristics])] = Some(fromEntityRelationship(r))
  }

  // scalar datatype
  
  object ModelScalarDataType {
    def apply(iri: omf#IRI): omf#ModelScalarDataType = makeScalarDataType(iri)
    def unapply(dt: omf#ModelScalarDataType): Some[omf#IRI] = Some(fromScalarDataType(dt))
  }

  // structured datatype
  
  object ModelStructuredDataType {
    def apply(iri: omf#IRI): omf#ModelStructuredDataType = makeStructuredDataType(iri)
    def unapply(dt: omf#ModelStructuredDataType): Some[omf#IRI] = Some(fromStructuredDataType(dt))
  }

  // structured data relationship

  object ModelStructuredDataRelationship {
    def apply(iri: omf#IRI, source: omf#ModelStructuredDataType, target: omf#ModelDataTypeDefinition): omf#ModelStructuredDataRelationship = makeStructuredDataRelationship(iri, source, target)
    def unapply(sd: omf#ModelStructuredDataRelationship): Some[(omf#IRI, omf#ModelStructuredDataType, omf#ModelDataTypeDefinition)] = Some(fromStructuredDataRelationship(sd))
  }

  // entity data relationship

  object ModelEntityDataRelationship {
    def apply(iri: omf#IRI, source: omf#ModelEntityDefinition, target: omf#ModelDataTypeDefinition): omf#ModelEntityDataRelationship = makeEntityDataRelationship(iri, source, target)
    def unapply(ed: omf#ModelEntityDataRelationship): Some[(omf#IRI, omf#ModelEntityDefinition, omf#ModelDataTypeDefinition)] = Some(fromEntityDataRelationship(ed))
  }

  // entity concept subclass axiom

  object EntityConceptSubClassAxiom {
    def apply(sub: omf#ModelEntityConcept, sup: omf#ModelEntityConcept): omf#EntityConceptSubClassAxiom = makeEntityConceptSubClassAxiom(sub, sup)
    def unapply(ax: omf#EntityConceptSubClassAxiom): Some[(omf#ModelEntityConcept, omf#ModelEntityConcept)] = Some(fromEntityConceptSubClassAxiom(ax))
  }
  
  // entity concept restriction axiom

  object EntityConceptRestrictionAxiom {
    def apply(sub: omf#ModelEntityConcept, rel: omf#ModelEntityRelationship, range: omf#ModelEntityDefinition): omf#EntityConceptRestrictionAxiom = makeEntityConceptRestrictionAxiom(sub, rel, range)
    def unapply(ax: omf#EntityConceptRestrictionAxiom): Some[(omf#ModelEntityConcept, omf#ModelEntityRelationship, omf#ModelEntityDefinition)] = Some(fromEntityConceptRestrictionAxiom(ax))
  }
  
  // entity relationship subclass axiom

  object EntityRelationshipSubClassAxiom {
    def apply(sub: omf#ModelEntityRelationship, sup: omf#ModelEntityRelationship): omf#EntityRelationshipSubClassAxiom = makeEntityRelationshipSubClassAxiom(sub, sup)
    def unapply(ax: omf#EntityRelationshipSubClassAxiom): Some[(omf#ModelEntityRelationship, omf#ModelEntityRelationship)] = Some(fromEntityRelationshipSubClassAxiom(ax))
  }
    
  // scalar datatype facet restriction axiom

  // @TODO
  
  // instance graph

  object ModelInstanceGraph {
    def apply(iri: omf#IRI,
              t: Iterable[omf#ModelTerminologyGraph],
              c: Iterable[omf#ModelInstanceObject],
              r: Iterable[omf#ModelInstanceRelation],
              dl: Iterable[omf#ModelInstanceDataLiteral],
              ic: Iterable[omf#ModelInstanceDataStructure],
              sdp: Iterable[omf#ModelStructuredDataProperty],
              edp: Iterable[omf#ModelEntityDataProperty]): omf#ModelInstanceGraph = makeInstanceGraph(iri, t, c, r, dl, ic, sdp, edp)
    def unapply(ig: omf#ModelInstanceGraph): Some[(omf#IRI, Iterable[omf#ModelTerminologyGraph], Iterable[omf#ModelInstanceObject], Iterable[omf#ModelInstanceRelation], Iterable[omf#ModelInstanceDataLiteral], Iterable[omf#ModelInstanceDataStructure], Iterable[omf#ModelStructuredDataProperty], Iterable[omf#ModelEntityDataProperty])] = Some(fromInstanceGraph(ig))
  }

  // instance object
  
  object ModelInstanceObject {
    def apply(iri: omf#IRI, conceptType: omf#ModelEntityConcept): omf#ModelInstanceObject = makeInstanceObject(iri, conceptType)
    def unapply(o: omf#ModelInstanceObject): Some[(omf#IRI, omf#ModelEntityConcept)] = Some(fromInstanceObject(o))
  }

  // instance relation
  
  object ModelInstanceRelation {
    def apply(iri: omf#IRI, relationshipType: omf#ModelEntityRelationship, source: omf#ModelEntityInstance, target: omf#ModelEntityInstance): omf#ModelInstanceRelation = makeInstanceRelation(iri, relationshipType, source, target)
    def unapply(r: omf#ModelInstanceRelation): Some[(omf#IRI, omf#ModelEntityRelationship, omf#ModelEntityInstance, omf#ModelEntityInstance)] = Some(fromInstanceRelation(r))
  }

  // data literal

  object ModelInstanceDataLiteral {
    def apply(lexicalForm: String, datatype: omf#ModelScalarDataType): omf#ModelInstanceDataLiteral = makeDataLiteral(lexicalForm, datatype)
    def unapply(dl: omf#ModelInstanceDataLiteral): Some[(String, omf#ModelScalarDataType)] = Some(fromDataLiteral(dl))
  }

  // data structure

  object ModelInstanceDataStructure {
    def apply(iri: omf#IRI, datatype: omf#ModelStructuredDataType): omf#ModelInstanceDataStructure = makeDataStructure(iri, datatype)
    def unapply(ds: omf#ModelInstanceDataStructure): Some[(omf#IRI, omf#ModelStructuredDataType)] = Some(fromDataStructure(ds))
  }

  // structured data property

  object ModelStructuredDataProperty {
    def apply(ds: omf#ModelInstanceDataStructure, structuredDataRelationshipType: omf#ModelStructuredDataRelationship, value: omf#ModelDataInstance): omf#ModelStructuredDataProperty = makeStructuredDataProperty(ds, structuredDataRelationshipType, value)
    def unapply(sdp: omf#ModelStructuredDataProperty): Some[(omf#ModelInstanceDataStructure, omf#ModelStructuredDataRelationship, omf#ModelDataInstance)] = Some(fromStructuredDataProperty(sdp))
  }

  // entity data property

  object ModelEntityDataProperty {
    def apply(e: omf#ModelEntityInstance, entityDataRelationshipType: omf#ModelEntityDataRelationship, value: omf#ModelDataInstance): omf#ModelEntityDataProperty = makeEntityDataProperty(e, entityDataRelationshipType, value)
    def unapply(edp: omf#ModelEntityDataProperty): Some[(omf#ModelEntityInstance, omf#ModelEntityDataRelationship, omf#ModelDataInstance)] = Some(fromEntityDataProperty(edp))
  }
}