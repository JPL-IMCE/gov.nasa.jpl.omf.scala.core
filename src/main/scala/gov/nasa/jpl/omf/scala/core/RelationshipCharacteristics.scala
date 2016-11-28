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

import scala.{Enumeration,Int}
import scala.collection.immutable.Iterable

object RelationshipCharacteristics extends Enumeration {
  type RelationshipCharacteristics = Value
  val
  isFunctional,
  isInverseFunctional,
  isSymmetric,
  isAsymmetric,
  isReflexive,
  isIrreflexive,
  isTransitive,
  isEssential,
  isInverseEssential = Value

  def relationshipCharacteristicsSummary(c: Iterable[RelationshipCharacteristics]): Int = {
    def loop(summary: Int, rc: RelationshipCharacteristics): Int = summary + (rc match {
      case `isFunctional`        => 0x1
      case `isInverseFunctional` => 0x2
      case `isSymmetric`         => 0x4
      case `isAsymmetric`        => 0x8
      case `isReflexive`         => 0x10
      case `isIrreflexive`       => 0x20
      case `isTransitive`        => 0x40
      case `isEssential`         => 0x80
      case `isInverseEssential`  => 0x100
    })
    (0 /: c)(loop)
  }

}