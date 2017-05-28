package gov.nasa.jpl.omf.scala.core

import java.util.UUID
import scala.Predef.String

case class ScalarOneOfLiteralSignature[omf <: OMF]
( uuid: UUID,
  restriction: omf#ScalarOneOfRestriction,
  value: String
)
