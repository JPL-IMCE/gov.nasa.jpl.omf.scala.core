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
 */

package gov.nasa.jpl.omf.scala.core

import gov.nasa.jpl.imce.oml.resolver
import scala.Option

case class SegmentPredicateSignature[omf <: OMF[omf]]
(uuid: resolver.api.taggedTypes.SegmentPredicateUUID,
 bodySegment: omf#RuleBodySegment,
 predicate: Option[omf#Predicate],
 reifiedRelationshipSource: Option[omf#ReifiedRelationship],
 reifiedRelationshipInverseSource: Option[omf#ReifiedRelationship],
 reifiedRelationshipTarget: Option[omf#ReifiedRelationship],
 reifiedRelationshipInverseTarget: Option[omf#ReifiedRelationship],
 unreifiedRelationshipInverse: Option[omf#UnreifiedRelationship])