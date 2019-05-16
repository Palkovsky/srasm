package parser

import compiler.{InstructionSet}

object Lang {

  val ONE_LINE_COMMENT: String = ";"
  val MULTI_LINE_COMMENT_OPEN: String = "/*"
  val MULTI_LINE_COMMENT_CLOSE: String = "*/"

  val ORG_DIRECTIVE: String = "ORG"
  val DB_DIRECTIVE: String = "DB"
  val DUP_DIRECTIVE: String = "DUP"
  val STR_DIRECTIVE: String = "STR"

  val SEGMENTS: Array[String] = Array(
    "DATA",
    "CODE"
  )

  val DIRECTIVES: Array[String] = Array(
    ORG_DIRECTIVE,
    DB_DIRECTIVE,
    DUP_DIRECTIVE,
    STR_DIRECTIVE
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
