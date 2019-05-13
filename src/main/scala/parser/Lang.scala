package parser

import compiler.{InstructionSet}

object Lang {

  val ONE_LINE_COMMENT: String = ";"
  val MULTI_LINE_COMMENT_OPEN: String = "/*"
  val MULTI_LINE_COMMENT_CLOSE: String = "*/"

  val SEGMENTS: Array[String] = Array(
    "DATA",
    "CODE",
    "IRQ",
    "NMI"
  )

  val DIRECTIVES: Array[String] = Array(
    "ORG",
    "DB",
    "DUP"
  )

  val REGISTERS: Array[String] = Array(
    "X",
    "Y",
    "A"
  )

  val INSTRUCTIONS: Array[String] = InstructionSet.instructionsStr

  def isInstruction(str: String): Boolean = INSTRUCTIONS.contains(str)
  def isDirective(str: String): Boolean = DIRECTIVES.contains(str)
  def isSegment(str: String): Boolean = SEGMENTS.contains(str)
}
