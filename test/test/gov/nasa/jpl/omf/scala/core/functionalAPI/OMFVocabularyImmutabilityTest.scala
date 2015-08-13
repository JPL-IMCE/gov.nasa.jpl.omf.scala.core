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
package test.gov.nasa.jpl.omf.scala.core.functionalAPI

import gov.nasa.jpl.omf.scala.core.RelationshipCharacteristics._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import gov.nasa.jpl.omf.scala.core._
import org.scalatest._

import scala.language.{implicitConversions, postfixOps}

abstract class OMFVocabularyImmutabilityTest[omf <: OMF](
                                                          val saveStore: omf#Store, saveOps: OMFOps[omf],
                                                          val loadStore: omf#Store, loadOps: OMFOps[omf])
  extends WordSpec with Matchers {

  def preOMFSave(): Unit

  def postOMFSave(): Unit

  def withOMFSave(testCode: (omf#Store, OMFOps[omf]) => Any): Unit = {

    try {
      preOMFSave()
      testCode(saveStore, saveOps)
    } finally {
      postOMFSave()
    }
  }

  def preOMFLoad(): Unit

  def postOMFLoad(): Unit

  def withOMFLoad(testCode: (omf#Store, OMFOps[omf]) => Any): Unit = {

    try {
      preOMFLoad()
      testCode(loadStore, loadOps)
    } finally {
      postOMFLoad()
    }
  }

  "vocabulary roundtrip test" when {

    "construct tboxes and save them" in withOMFSave { (s, o) =>

      implicit val store = s
      implicit val ops = o
      import ops._

      val xsd = loadTerminologyGraph(makeIRI("http://www.w3.org/2001/XMLSchema"))
      xsd should be a 'success

      val integer =
        lookupScalarDataType(xsd.get._1, makeIRI("http://www.w3.org/2001/XMLSchema#integer"), recursively = false)
      integer.isDefined should be(true)

      val string =
        lookupScalarDataType(xsd.get._1, makeIRI("http://www.w3.org/2001/XMLSchema#string"), recursively = false)
      string.isDefined should be(true)

      val base = makeTerminologyGraph(
                                       makeIRI("http://imce.jpl.nasa.gov/foundation/base/base"),
                                       isDefinition)
      base should be a 'success

    {
      val base_extends_xsd = addTerminologyGraphExtension(base.get, xsd.get._1)
      base_extends_xsd should be a 'success

      val identifiedElement = addEntityAspect(base.get, "IdentifiedElement")
      identifiedElement should be a 'success

      val hasIdentifier = addDataRelationshipFromEntityToScalar(
                                                                 graph = base.get,
                                                                 source = identifiedElement.get,
                                                                 target = string.get,
                                                                 dataRelationshipName = "hasIdentifier")
      hasIdentifier should be a 'success

    }

      val ibase = asImmutableTerminologyGraph(base.get)
      ibase should be a 'success

      val identifiedElement =
        lookupEntityAspect(
                            ibase.get._1,
                            makeIRI("http://imce.jpl.nasa.gov/foundation/base/base#IdentifiedElement"),
                            recursively = false)
      identifiedElement.isDefined should be(true)

      val mission = makeTerminologyGraph(
                                          makeIRI("http://imce.jpl.nasa.gov/foundation/mission/mission"),
                                          isDefinition)
      mission should be a 'success

      val mission_extends_base = addTerminologyGraphExtension(mission.get, ibase.get._1)
      mission_extends_base should be a 'success

      val component = addEntityConcept(mission.get, "Component", isAbstract = false)
      component should be a 'success

      val component_extends_identifiedElement = addEntityDefinitionAspectSubClassAxiom(
                                                                                        graph = mission.get,
                                                                                        sub = component.get,
                                                                                        sup = identifiedElement.get)
      component_extends_identifiedElement.isSuccess should be(true)

      val function = addEntityConcept(mission.get, "Function", isAbstract = false)
      function should be a 'success

      val function_extends_identifiedElement = addEntityDefinitionAspectSubClassAxiom(
                                                                                       graph = mission.get,
                                                                                       sub = function.get,
                                                                                       sup = identifiedElement.get)
      function_extends_identifiedElement should be a 'success

      val component_performs_function = addEntityReifiedRelationship(
                                                                      graph = mission.get,
                                                                      source = component.get,
                                                                      target = function.get,
                                                                      characteristics = List(isAsymmetric, isIrreflexive, isInverseFunctional),
                                                                      reifiedRelationshipName = "Performs",
                                                                      unreifiedRelationshipName = "performs",
                                                                      unreifiedInverseRelationshipName = Some("isPerformedBy"),
                                                                      isAbstract = false)
      component_performs_function should be a 'success

      val item = addEntityConcept(mission.get, "Item", isAbstract = false)
      item should be a 'success

      val message = addEntityConcept(mission.get, "Message", isAbstract = false)
      message should be a 'success

      val materialItem = addEntityConcept(mission.get, "MaterialItem", isAbstract = false)
      materialItem should be a 'success

      val message_extends_item =
        addEntityConceptSubClassAxiom(mission.get, message.get, item.get)
      message_extends_item should be a 'success

      val materialItem_extends_item =
        addEntityConceptSubClassAxiom(mission.get, materialItem.get, item.get)
      materialItem_extends_item should be a 'success

      val baseSaved = saveTerminologyGraph(base.get)
      baseSaved should be a 'success

      val missionSaved = saveTerminologyGraph(mission.get)
      missionSaved should be a 'success

                                                    }

    "read tboxes and check them" in withOMFLoad { (s, o) =>

      implicit val store = s
      implicit val ops = o
      import ops._

      val xsd =
        loadTerminologyGraph(makeIRI("http://www.w3.org/2001/XMLSchema"))
      xsd should be a 'success

      val integer =
        lookupScalarDataType(xsd.get._1, makeIRI("http://www.w3.org/2001/XMLSchema#integer"), recursively = false)
      integer.isDefined should be(true)

      val base =
        loadTerminologyGraph(makeIRI("http://imce.jpl.nasa.gov/foundation/base/base"))
      base should be a 'success

    {
      val s = ops.fromTerminologyGraph(base.get._1)
      s.imports.isEmpty should be(false)
      s.imports.toSet.contains(xsd.get._1) should be(true)
      s.aspects.isEmpty should be(false)
      s.concepts.isEmpty should be(true)
      s.reifiedRelationships.isEmpty should be(true)
      s.scalarDataTypes.isEmpty should be(true)
      s.structuredDataTypes.isEmpty should be(true)
      s.entity2scalarDataRelationships.isEmpty should be(false)
      s.entity2structureDataRelationships.isEmpty should be(true)
      s.structure2scalarDataRelationships.isEmpty should be(true)
      s.structure2structureDataRelationships.isEmpty should be(true)
      s.axioms.isEmpty should be(true)
    }

      val string =
        lookupScalarDataType(base.get._1, makeIRI("http://www.w3.org/2001/XMLSchema#string"), recursively = true)
      string.isDefined should be(true)

      val identifiedElement =
        lookupEntityAspect(
                            base.get._1,
                            makeIRI("http://imce.jpl.nasa.gov/foundation/base/base#IdentifiedElement"), recursively = false)
      identifiedElement.isDefined should be(true)

      val hasIdentifier =
        lookupEntityDataRelationshipFromEntityToScalar(
                                                        base.get._1,
                                                        makeIRI("http://imce.jpl.nasa.gov/foundation/base/base#hasIdentifier"), recursively = false)
      hasIdentifier.isDefined should be(true)

      val (_, hasIdentifierSource, hasIdentifierTarget) =
        fromDataRelationshipFromEntityToScalar(hasIdentifier.get)
      identifiedElement.get should be(hasIdentifierSource)
      string.get should be(hasIdentifierTarget)

      val mission =
        loadTerminologyGraph(makeIRI("http://imce.jpl.nasa.gov/foundation/mission/mission"))
      mission should be a 'success

    {
      val s = ops.fromTerminologyGraph(mission.get._1)
      s.imports.isEmpty should be(false)
      s.imports.toSet.contains(base.get._1) should be(true)
      s.aspects.isEmpty should be(true)
      s.concepts.isEmpty should be(false)
      s.reifiedRelationships.isEmpty should be(false)
      s.scalarDataTypes.isEmpty should be(true)
      s.structuredDataTypes.isEmpty should be(true)
      s.entity2scalarDataRelationships.isEmpty should be(true)
      s.entity2structureDataRelationships.isEmpty should be(true)
      s.structure2scalarDataRelationships.isEmpty should be(true)
      s.structure2structureDataRelationships.isEmpty should be(true)
      s.axioms.isEmpty should be(false)
    }

      val component = lookupEntityConcept(
                                           mission.get._1,
                                           makeIRI("http://imce.jpl.nasa.gov/foundation/mission/mission#Component"),
                                           recursively = false)
      component.isDefined should be(true)

      val function = lookupEntityConcept(
                                          mission.get._1,
                                          makeIRI("http://imce.jpl.nasa.gov/foundation/mission/mission#Function"),
                                          recursively = false)
      function.isDefined should be(true)

      val component_performs_function = lookupEntityReifiedRelationship(
                                                                         mission.get._1,
                                                                         makeIRI("http://imce.jpl.nasa.gov/foundation/mission/mission#Performs"),
                                                                         recursively = false)
      component_performs_function.isDefined should be(true)

      val component_performs_function_info =
        fromEntityReifiedRelationship(component_performs_function.get)
      component_performs_function_info.source should be(component.get)
      component_performs_function_info.target should be(function.get)
      component_performs_function_info.isAbstract should be(false)
                                                }

  }

}