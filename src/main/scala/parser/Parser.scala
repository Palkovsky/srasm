package parser

sealed abstract class ASTNode

case class Instruction(inst: String) extends ASTNode
case class Block(Instructions: Seq[ASTNode]) extends ASTNode

case class NumberLiteral(num: Long) extends ASTNode
case class LabelLiteral(label: String) extends ASTNode
case class ListLiteral(bytes: Seq[NumberLiteral]) extends ASTNode

case class Dereference(addr: NumberLiteral) extends ASTNode
case class LabelAssigment(key: String, dest: ASTNode) extends ASTNode

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
    }

    val node = pair._1
    val tail = pair._2

    parse(tail, node :: acc)
  }

  private def parseNum(numStr: String): Long = 2137
}
