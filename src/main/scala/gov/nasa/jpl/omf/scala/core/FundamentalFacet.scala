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
  * @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#rf-fund-facets
  */
sealed abstract trait FundamentalFacet

object FundamentalFacet {

  /**
    * @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#rf-ordered
    */
  object OrderedConstraint extends Enumeration {
    type OrderedConstraint = Value
    val _false, partial, total = Value
  }

  import OrderedConstraint._

  /**
    * @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#rf-ordered
    */
  case class ordered(constraint: OrderedConstraint)
    extends FundamentalFacet

  /**
    * @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#rf-bounded
    */
  case class bounded(constraint: Boolean)
    extends FundamentalFacet

  /**
    * @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#rf-cardinality
    */
  object CardinalityConstraint extends Enumeration {
    type CardinalityConstraint = Value
    val finite, countablyInfinite = Value
  }

  import CardinalityConstraint._

  /**
    * @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#rf-cardinality
    */
  case class cardinality(constraint: CardinalityConstraint)
    extends FundamentalFacet

  /**
    * @see http://www.w3.org/TR/2012/REC-xmlschema11-2-20120405/datatypes.html#rf-numeric
    */
  case class numeric(constraint: Boolean)
    extends FundamentalFacet
}