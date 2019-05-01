package parser

object Lang {

  val SPACE = new String(Array[Byte](0x20))
  val NEWLINE = new String(Array[Byte](0x0A))
  val CRLF = new String(Array[Byte](0x0D, 0x0A))
  val CR =  new String(Array[Byte](0x0D))

  val ADDR_ASSIGMENT = "<="

  val DEREF_PARENS_OPEN = "["
  val DEREF_PARENS_CLOSE = "]"

  val BLOCK_PARENS_OPEN = "{"
  val BLOCK_PARENS_CLOSE = "}"

  val DIRECTIVE_PARENS_OPEN = "("
  val DIRECTIVE_PARENS_CLOSE = ")"

  val ARG_SEPARATOR = ","

  val KEYWORDS: Array[String] = Array(
    "DATA", "CODE"
  )

  val DIRECTIVES: Array[String] = Array(
    "ORG"
  )

  val REGISTERS: Array[String] = Array(
    "X", "Y", "A"
  )

  val INSTRUCTIONS: Array[String] = Array(
    "BRK", "JMP",
    "LDA", "LDX", "LDY",
    "TXS"
  )

  def isInstruction(str: String): Boolean =
    INSTRUCTIONS.contains(str.toUpperCase())

  def isDirective(str: String): Boolean =
    DIRECTIVES.contains(str.toUpperCase())

  def isRegister(str: String): Boolean =
    REGISTERS.contains(str.toUpperCase())
}
