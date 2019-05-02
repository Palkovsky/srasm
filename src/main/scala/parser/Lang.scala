package parser

import compiler.{InstructionSet}

object Lang {
  val SEGMENTS: Array[String] = Array(
    "DATA",
    "CODE"
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
}
