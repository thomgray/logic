package com.gray.logic.tools
import com.gray.logic.language.FormulaWriterAlphabetic
import grizzled.slf4j.{Logging => GrizzleLogging}

trait Logging extends GrizzleLogging {
  implicit val writer = FormulaWriterAlphabetic
}
