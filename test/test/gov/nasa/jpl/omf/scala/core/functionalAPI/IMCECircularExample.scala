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

package test.gov.nasa.jpl.omf.scala.core.functionalAPI

import gov.nasa.jpl.imce.oml.tables.taggedTypes.localName
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import gov.nasa.jpl.omf.scala.core._
import scala.{Some, StringContext, Unit}
import org.scalatest.exceptions.TestFailedException
import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.Set
import scala.util.control.Exception.nonFatalCatch
import scalaz.\/

abstract class IMCECircularExample[omf <: OMF[omf]](val saveStore: omf#Store,
                                                    saveOps: OMFOps[omf],
                                                    val loadStore: omf#Store,
                                                    loadOps: OMFOps[omf])
    extends WordSpec
    with Matchers {

  def preOMFSave(): Unit

  def postOMFSave(): Unit

  def withOMFSave(
      testCode: (omf#Store, OMFOps[omf]) => Set[java.lang.Throwable] \/ Unit)
    : Unit =
    nonFatalCatch[Unit]
      .withApply { (cause: java.lang.Throwable) =>
        throw new TestFailedException(
          message = s"withOMFSave failed: ${cause.getMessage}",
          cause = cause,
          failedCodeStackDepth = 2)
      }
      .apply({
        preOMFSave()
        val result = testCode(saveStore, saveOps)
        postOMFSave()
        result.leftMap { errors =>
          throw new TestFailedException(
            message = Some(s"withOMFSave ${errors.size} errors"),
            cause = errors.headOption,
            failedCodeStackDepth = 1
          )
        }
        ()
      })

  def preOMFLoad(): Unit

  def postOMFLoad(): Unit

  def withOMFLoad(
      testCode: (omf#Store, OMFOps[omf]) => Set[java.lang.Throwable] \/ Unit)
    : Unit =
    nonFatalCatch[Unit]
      .withApply { (cause: java.lang.Throwable) =>
        throw new TestFailedException(
          message = s"withOMFLoad failed: ${cause.getMessage}",
          cause = cause,
          failedCodeStackDepth = 2)
      }
      .apply({
        preOMFLoad()
        val result = testCode(loadStore, loadOps)
        postOMFLoad()
        result.leftMap { errors =>
          throw new TestFailedException(
            message = Some(s"withOMFLoad ${errors.size} errors"),
            cause = errors.headOption,
            failedCodeStackDepth = 1
          )
        }
        ()
      })

  "circular construction tests" when {
    "construct circular tboxes and save them" in withOMFSave { (s, o) =>
      implicit val store = s
      implicit val ops = o
      import ops._

      for {
        a_iri <- makeIRI("http://imce.jpl.nasa.gov/example/A")
        m_a <- makeTerminologyGraph(a_iri, isOpenWorld)

        b_iri <- makeIRI("http://imce.jpl.nasa.gov/example/B")
        m_b <- makeTerminologyGraph(b_iri, isOpenWorld)

        b_extends_a <- addTerminologyExtension(m_b, m_a)

        a_extends_b <- addTerminologyExtension(m_a, m_b)

        p <- addConcept(m_a, localName("P"))
        q <- addConcept(m_b, localName("Q"))
        p_is_q <- addConceptSpecializationAxiom(m_a, p, q)
        q_is_p <- addConceptSpecializationAxiom(m_a, q, p)

        a_saved = saveTerminology(m_a)

        b_saved = saveTerminology(m_b)
      } yield ()
    }

    "load circular tboxes" in withOMFLoad { (s, o) =>
      implicit val store = s
      implicit val ops = o
      import ops._

      for {
        a_iri <- makeIRI("http://imce.jpl.nasa.gov/example/A")
        b_iri <- makeIRI("http://imce.jpl.nasa.gov/example/B")
        drc <- loadBuiltinDatatypeMap()
        a_table <- loadTerminology(initializeOntologyMapping(drc), a_iri)

      } yield ()
    }
  }
}
