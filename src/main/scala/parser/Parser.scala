package parser

sealed abstract class ASTNode

case class Instruction(inst: String, arg1: ASTNode = null, arg2: ASTNode = null) extends ASTNode
case class Block(Instructions: Seq[ASTNode]) extends ASTNode
case class DirectiveCall(name: String, args: Array[String]) extends ASTNode

trait Literal extends ASTNode
case class NumberLiteral(num: Long) extends ASTNode with Literal
case class LabelLiteral(label: String) extends ASTNode with Literal

case class Dereference(addr: NumberLiteral) extends ASTNode
case class BlockAssigment(label: String, dest: Block) extends ASTNode
case class LiteralAssigment(label: String, dest: Literal) extends ASTNode

class Parser() {

  def parse(tokens: List[Token]): List[ASTNode] = parse(tokens, List())

  private def parse(tokens: List[Token], acc: List[ASTNode]): List[ASTNode] = {

    // Stop conditions
    tokens match {
      case List() => return List()
      case List(EOFToken()) => return List()
    }

    // Actual parsing
    val pair: (ASTNode, List[Token]) = tokens match {

      case BlockToken(tokens) :: tail => (Block(parse(tokens, List())), tail)

      case SpecialToken(Lang.DEREF_PARENS_OPEN) :: StringToken(numString) :: SpecialToken(Lang.DEREF_PARENS_CLOSE) :: tail => {
        val parsed: Long = parseNum(numString)
        // Exceptions: string is not a number
        val deref: Dereference = Dereference(NumberLiteral(parsed))
        (deref, tail)
      }

      // Block assigment
      case StringToken(label) :: SpecialToken(Lang.ADDR_ASSIGMENT) :: BlockToken(tokens) :: tail => {
        (BlockAssigment(label, (Block(parse(tokens, List())))), tail)
      }

      // Data assigment
      case StringToken(label) :: SpecialToken(Lang.ADDR_ASSIGMENT) :: tail => {
        val parsed: (Literal, List[Token]) = parseData(tail)
        (LiteralAssigment(label, parsed._1), parsed._2)
      }

      // Directive call(0-ary)
      case StringToken(directiveName) ::SpecialToken(Lang.DIRECTIVE_PARENS_OPEN) :: SpecialToken(Lang.DIRECTIVE_PARENS_CLOSE) :: tail => {
        (DirectiveCall(directiveName, Array()), tail)
      }
      // Directive call(1-ary)
      case StringToken(directiveName) :: SpecialToken(Lang.DIRECTIVE_PARENS_OPEN) :: StringToken(arg1) :: SpecialToken(Lang.DIRECTIVE_PARENS_CLOSE) :: tail => {
        (DirectiveCall(directiveName, Array(arg1)), tail)
      }

      
    }

    val node = pair._1
    val tail = pair._2

    parse(tail, node :: acc)
  }

  private def parseNum(numStr: String): Long = 2137
  private def parseData(tokens: List[Token]): (Literal, List[Token]) = (null, tokens)
}
