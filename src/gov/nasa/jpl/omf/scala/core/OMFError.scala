/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2016, California Institute of Technology ("Caltech").
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

import scala.{Option,None}
import scala.Predef.String
import scalaz._, Scalaz._

object OMFError {

  type ThrowableNel = NonEmptyList[java.lang.Throwable]
  type OptionThrowableNel = Option[ThrowableNel]
  val emptyThrowableNel = Option.empty[NonEmptyList[java.lang.Throwable]]

  class OMFException
  ( val message: String,
    val cause: OptionThrowableNel = emptyThrowableNel )
    extends java.lang.Throwable(message) {

    cause.map { nels =>
      this.initCause(nels.head)
    }

  }

  class OMFBindingException
  ( override val message: String,
    override val cause: OptionThrowableNel = emptyThrowableNel )
    extends OMFException(message, cause)

  class OMFOpsException[Omf <: OMF]
  ( val ops: OMFOps[Omf],
    override val message: String,
    override val cause: OptionThrowableNel = emptyThrowableNel )
    extends OMFException(message, cause)

  def omfError
  ( message: String )
  : java.lang.Throwable =
  new OMFException(message)

  def omfException
  ( message: String,
    cause: OptionThrowableNel = emptyThrowableNel )
  : java.lang.Throwable =
    new OMFException(message, cause)

  def omfException
  ( message: String,
    cause: java.lang.Throwable )
  : java.lang.Throwable =
    new OMFException(message, cause.wrapNel.some)

  def omfBindingError
  ( message: String )
  : java.lang.Throwable =
  new OMFBindingException( message )

  def omfBindingException
  ( message: String,
    cause: OptionThrowableNel = emptyThrowableNel )
  : java.lang.Throwable =
    new OMFBindingException( message, cause )

  def omfBindingException
  ( message: String,
    cause: java.lang.Throwable  )
  : java.lang.Throwable =
    new OMFBindingException( message, cause.wrapNel.some )

  def omfOpsError[Omf <: OMF]
  ( ops: OMFOps[Omf],
    message: String )
  : java.lang.Throwable =
    new OMFOpsException( ops, message )

  def omfOpsException[Omf <: OMF]
  ( ops: OMFOps[Omf],
    message: String,
    cause: OptionThrowableNel = emptyThrowableNel )
  : java.lang.Throwable =
    new OMFOpsException( ops, message, cause )

  def omfOpsException[Omf <: OMF]
  ( ops: OMFOps[Omf],
    message: String,
    cause: java.lang.Throwable  )
  : java.lang.Throwable =
    new OMFOpsException( ops, message, cause.wrapNel.some )

}