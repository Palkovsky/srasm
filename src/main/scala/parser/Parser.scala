package parser

sealed abstract class ASTNode

case class Instruction(inst: String) extends ASTNode
case class Block(Instructions: Seq[Instruction]) extends ASTNode

case class NumberLiteral(num: Long) extends ASTNode
case class ListLiteral(bytes: Seq[NumberLiteral]) extends ASTNode

case class Dereference(addr: NumberLiteral) extends ASTNode
case class Label(key: String, dest: ASTNode) extends ASTNode

class Parser() {

  def parse(tokens: Seq[Token]): Unit = {
    val head: Token = tokens.head
    val tail: Seq[Token] = tokens.tail

    head match {
      case SpecialToken(" ") => return parse(tail)
      case SpecialToken("{") => {

      }
      case SpecialToken("[") => {

      }
    }

    ()
  }
}
