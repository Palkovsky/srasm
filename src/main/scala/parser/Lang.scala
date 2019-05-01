package parser

object Lang {

  val SPACE = " "
  val NEWLINE = "\n"
  val CRLF = "\r\n"

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

  val INSTRUCTIONS: Array[String] = Array(
    "BRK", "JMP",
    "LDA", "LDX", "LDY",
    "TXS"
  )

  def isInstruction(str: String): Boolean =
    INSTRUCTIONS.contains(str.toUpperCase())

  def isDirective(str: String): Boolean =
    DIRECTIVES.contains(str.toUpperCase())
}
