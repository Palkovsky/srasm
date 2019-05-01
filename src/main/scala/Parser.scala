import java.lang.{Long}
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

sealed abstract class ASTNode
case class RootNode(nodes: Seq[ASTNode]) extends ASTNode
case class Instruction(inst: String, arg1: ASTNode = null, arg2: ASTNode = null) extends ASTNode
case class LabelDefinition(value: String) extends ASTNode
case class Label(value: String) extends ASTNode
case class Register(value: String) extends ASTNode
case class Number(value: Long) extends ASTNode
case class Dereference(value: ASTNode) extends ASTNode
case class Unknown(value: String) extends ASTNode

object ASMParser extends RegexParsers {

  private def alternative(seq: Seq[String]): Parser[String] = {
    seq.foldLeft[Parser[String]](seq.head)((acc: Parser[String], value: String) => acc | value)
  }

  //override protected val whiteSpace: Regex = "\\s".r
  override def skipWhitespace: Boolean = true

  private def hexPrefix: Parser[String] = "#"
  private def derefPrefix: Parser[String] = "$"
  private def argSeparator: Parser[String] = ","

  private def register: Parser[Register] = alternative(Lang.REGISTERS) ^^ {str => Register(str)}
  private def instructionCode: Parser[String] = alternative(Lang.INSTRUCTIONS) | alternative(Lang.DIRECTIVES)

  private def any: Parser[Unknown] = ".".r ^^ { str => Unknown(str) }
  private def decimalNumber: Parser[Number] = "[0-9]+".r ^^ { str => Number(Long.parseLong(str, 10)) }
  private def hexadecimalNumber: Parser[Number] = hexPrefix ~ "[0-9a-fA-F]+".r ^^ {case _ ~ str => Number(Long.parseLong(str, 16)) }
  private def labelDefinition: Parser[LabelDefinition] = "[a-zA-Z0-9_]+:".r ^^ { str => LabelDefinition(str.substring(0, str.length()-1)) }
  private def label: Parser[Label] = not(instructionCode) ~> "[a-zA-Z0-9_]+".r  ^^ { str => Label(str)}

  private def address: Parser[ASTNode] = decimalNumber | hexadecimalNumber |  register | label
  private def dereference: Parser[Dereference] = derefPrefix ~ address ^^ {case _ ~ addr => Dereference(addr)}

  private def twoAryInstruction: Parser[Instruction] = instructionCode ~ (dereference | address) ~ argSeparator ~ (register)  ^^ {case inst ~ arg1 ~ _ ~ arg2 => Instruction(inst, arg1, arg2)}
  private def oneAryInstruction: Parser[Instruction] = instructionCode ~ (dereference | address)  ^^ {case inst ~ arg1 => Instruction(inst, arg1)}
  private def zeroAryInstruction: Parser[Instruction]  = instructionCode ^^ {case inst => Instruction(inst)}
  private def instruction: Parser[Instruction] = twoAryInstruction | oneAryInstruction | zeroAryInstruction

  private def directive: Parser[ASTNode] = instruction | labelDefinition | any

  private def asm: Parser[RootNode] = rep[ASTNode](directive) ^^ { nodes => RootNode(nodes) }

  def runParser(code: String): ParseResult[RootNode] = parse(asm, code)
}
