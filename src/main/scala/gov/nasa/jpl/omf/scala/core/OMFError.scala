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

import scala.collection.immutable.Set
import scala.Predef.String

object OMFError {

  type Throwables = Set[java.lang.Throwable]
  val emptyThrowables = Set[java.lang.Throwable]()

  class OMFException
  ( val message: String,
    val cause: Throwables )
    extends java.lang.Throwable(message) {

    cause.headOption.map { e =>
      this.initCause(e)
    }

  }

  class OMFBindingException
  ( override val message: String,
    override val cause: Throwables )
    extends OMFException(message, cause)

  class OMFOpsException[Omf <: OMF]
  ( val ops: OMFOps[Omf],
    override val message: String,
    override val cause: Throwables )
    extends OMFException(message, cause)

  def omfError
  ( message: String )
  : java.lang.Throwable =
  new OMFException(message, emptyThrowables)

  def omfException
  ( message: String,
    cause: Throwables )
  : java.lang.Throwable =
    new OMFException(message, cause)

  def omfException
  ( message: String,
    cause: java.lang.Throwable )
  : java.lang.Throwable =
    new OMFException(message, Set[java.lang.Throwable](cause))

  def omfBindingError
  ( message: String )
  : java.lang.Throwable =
  new OMFBindingException( message, emptyThrowables )

  def omfBindingException
  ( message: String,
    cause: Throwables )
  : java.lang.Throwable =
    new OMFBindingException( message, cause )

  def omfBindingException
  ( message: String,
    cause: java.lang.Throwable  )
  : java.lang.Throwable =
    new OMFBindingException( message, Set[java.lang.Throwable](cause))

  def omfOpsError[Omf <: OMF]
  ( ops: OMFOps[Omf],
    message: String )
  : java.lang.Throwable =
    new OMFOpsException( ops, message, emptyThrowables )

  def omfOpsException[Omf <: OMF]
  ( ops: OMFOps[Omf],
    message: String,
    cause: Throwables )
  : java.lang.Throwable =
    new OMFOpsException( ops, message, cause )

  def omfOpsException[Omf <: OMF]
  ( ops: OMFOps[Omf],
    message: String,
    cause: java.lang.Throwable  )
  : java.lang.Throwable =
    new OMFOpsException( ops, message, Set[java.lang.Throwable](cause))

}