import java.lang.{Long}
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

sealed abstract class ASTNode
case class RootNode(nodes: Seq[ASTNode]) extends ASTNode
case class Segment(name: String, nodes: Seq[ASTNode]) extends ASTNode
case class Instruction(inst: String, arg1: ASTNode = null, arg2: ASTNode = null) extends ASTNode
case class Indirect(inst: Instruction) extends ASTNode
case class IndirectX(inst: Instruction) extends ASTNode
case class IndirectY(inst: Instruction) extends ASTNode
case class LabelDefinition(value: String) extends ASTNode
case class Label(value: String) extends ASTNode
case class Register(value: String) extends ASTNode
case class Number(value: Long) extends ASTNode
case class Immediate(value: ASTNode) extends ASTNode
case class Unknown(value: String) extends ASTNode


object ASMParser extends RegexParsers {

  private def alternative(seq: Seq[String]): Parser[String] = {
    seq.foldLeft[Parser[String]](seq.head)((acc: Parser[String], value: String) => acc | value)
  }

  //override protected val whiteSpace: Regex = "\\s".r
  override def skipWhitespace: Boolean = true

  private def hexPrefix: Parser[String] = "$"
  private def immediatePrefix: Parser[String] = "#"
  private def argSeparator: Parser[String] = ","

  private def register: Parser[Register] = alternative(Lang.REGISTERS) ^^ {str => Register(str)}
  private def instructionCode: Parser[String] =
    alternative(Lang.INSTRUCTIONS) | alternative(Lang.INSTRUCTIONS.map(str => str.toLowerCase)) |
    alternative(Lang.DIRECTIVES) | alternative(Lang.DIRECTIVES.map(str => str.toLowerCase))
  private def segmentName: Parser[String] = alternative(Lang.SEGMENTS) | alternative(Lang.SEGMENTS.map(str => str.toLowerCase))

  private def any: Parser[Unknown] = ".".r ^^ { str => Unknown(str) }
  private def decimalNumber: Parser[Number] = "[0-9]+".r ^^ { str => Number(Long.parseLong(str, 10)) }
  private def hexadecimalNumber: Parser[Number] = hexPrefix ~ "[0-9a-fA-F]+".r ^^ {case _ ~ str => Number(Long.parseLong(str, 16)) }
  private def labelDefinition: Parser[LabelDefinition] = "[a-zA-Z0-9_]+:".r ^^ { str => LabelDefinition(str.substring(0, str.length()-1)) }
  private def label: Parser[Label] = not(instructionCode | segmentName) ~> "[a-zA-Z0-9_]+".r  ^^ { str => Label(str)}

  private def address: Parser[ASTNode] = decimalNumber | hexadecimalNumber |  register | label
  private def immediate: Parser[Immediate] = immediatePrefix ~ address ^^ {case _ ~ addr => Immediate(addr)}

  private def twoAryInstruction: Parser[Instruction] = instructionCode ~ (immediate | address) ~ argSeparator ~ register  ^^ {case inst ~ arg1 ~ _ ~ arg2 => Instruction(inst.toUpperCase(), arg1, arg2)}
  private def oneAryInstruction: Parser[Instruction] = instructionCode ~ (immediate | address)  ^^ {case inst ~ arg1 => Instruction(inst.toUpperCase(), arg1)}
  private def zeroAryInstruction: Parser[Instruction]  = instructionCode ^^ {case inst => Instruction(inst.toUpperCase())}

  // ex. JMP ($FFFF)
  private def indirectInstruction: Parser[Indirect] =
    instructionCode ~  "(" ~ (immediate | address)  ~ ")" ^^ {case inst ~ _ ~ arg1  ~ _  => Indirect(Instruction(inst.toUpperCase(), arg1))} // Ex: STA ($15,X)

  private def indirectXInstruction: Parser[IndirectX] =
   instructionCode ~  "(" ~ (immediate | address)  ~ argSeparator ~ register ~ ")" ^^ {case inst ~ _ ~ arg1 ~ _ ~ arg2 ~ _  => IndirectX(Instruction(inst.toUpperCase(), arg1, arg2))} // Ex: STA ($15,X)

  private def indirectYInstruction: Parser[IndirectY] =
    instructionCode ~ "(" ~ (immediate | address) ~  ")" ~ argSeparator ~ register ^^ {case inst ~ _ ~ arg1 ~ _ ~ _ ~ arg2 => IndirectY(Instruction(inst.toUpperCase(), arg1, arg2))} // Ex: STA ($15), Y

  private def instruction: Parser[Instruction] = twoAryInstruction | oneAryInstruction | zeroAryInstruction

  private def directive: Parser[ASTNode] = labelDefinition | indirectXInstruction | indirectYInstruction |  indirectInstruction | instruction
  
  private def segment: Parser[Segment] = segmentName ~ "{" ~ rep[ASTNode](directive) ~ "}" ^^ {case seg  ~ _ ~ nodes ~ _  => Segment(seg.toUpperCase(), nodes)}

  private def asm: Parser[RootNode] = rep[ASTNode](segment | directive) ^^ { nodes => RootNode(nodes) }

  def runParser(code: String): ParseResult[RootNode] = parse(asm, code)
}
