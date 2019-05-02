import compiler.{InstructionSet}

object Lang {
  val SEGMENTS: Array[String] = Array(
    "DATA",
    "CODE"
  )

  val DIRECTIVES: Array[String] = Array(
    "ORG",
    "DB"
  )

  val REGISTERS: Array[String] = Array(
    "X",
    "Y",
    "A"
  )

  val INSTRUCTIONS: Array[String] = InstructionSet.instructionsStr
}