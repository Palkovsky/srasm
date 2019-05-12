package parser

import java.lang.{Long}
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

sealed abstract class ASTNode
case class RootNode(nodes: List[ASTNode]) extends ASTNode
case class Segment(name: String, nodes: List[ASTNode]) extends ASTNode

case class InstructionNode(inst: String, arg1: ASTNode = null, arg2: ASTNode = null) extends ASTNode
case class Relative(inst: InstructionNode) extends ASTNode
case class Indirect(inst: InstructionNode) extends ASTNode
case class IndirectX(inst: InstructionNode) extends ASTNode
case class IndirectY(inst: InstructionNode) extends ASTNode

case class LabelDefinition(value: String) extends ASTNode
case class Label(value: String) extends ASTNode
case class Register(value: String) extends ASTNode
case class Number(value: Long) extends ASTNode
case class Immediate(value: ASTNode) extends ASTNode
case class Unknown(value: String) extends ASTNode

// Macros
case class MacroNumberDef(label: Label, number: Number) extends ASTNode

object ASMParser extends RegexParsers {

  private def alternative(seq: Seq[String]): Parser[String] = {
    seq.foldLeft[Parser[String]](seq.head)((acc: Parser[String], value: String) => acc | value)
  }

  //override protected val whiteSpace: Regex = "\\s".r
  override def skipWhitespace: Boolean = true

  private def hexPrefix: Parser[String] = "$"
  private def immediatePrefix: Parser[String] = "#"
  private def argSeparator: Parser[String] = ","
  private def commentSign: Parser[String] = ";"

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

  private def macroDef: Parser[MacroNumberDef] = "define" ~ label ~ (decimalNumber | hexadecimalNumber) ^^ {case _ ~ label ~ num => MacroNumberDef(label, num)}
  private def macros: Parser[MacroNumberDef] = macroDef

  private def address: Parser[ASTNode] = decimalNumber | hexadecimalNumber |  register | label
  private def immediate: Parser[Immediate] = immediatePrefix ~ address ^^ {case _ ~ addr => Immediate(addr)}

  private def twoAryInstruction: Parser[InstructionNode] =
    instructionCode ~ (immediate | address) ~ argSeparator ~ register  ^^ {case inst ~ arg1 ~ _ ~ arg2 => InstructionNode(inst.toUpperCase(), arg1, arg2)}

  private def oneAryInstruction: Parser[InstructionNode] =
    instructionCode ~ (immediate | address)  ^^ {case inst ~ arg1 => InstructionNode(inst.toUpperCase(), arg1)}

  private def zeroAryInstruction: Parser[InstructionNode]  =
    instructionCode ^^ {case inst => InstructionNode(inst.toUpperCase())}

  // ex. BNE *-4 or BNE *LABEL1
  private def relativeInstruction: Parser[Relative] =
    instructionCode ~ "*" ~ (immediate | address) ^^ {case inst ~ _ ~ arg1 => Relative(InstructionNode(inst.toUpperCase(), arg1)) }

  // ex. JMP ($FFFF)
  private def indirectInstruction: Parser[Indirect] =
    instructionCode ~  "(" ~ (immediate | address)  ~ ")" ^^ {case inst ~ _ ~ arg1  ~ _  => Indirect(InstructionNode(inst.toUpperCase(), arg1))} // Ex: STA ($15,X)

  // Ex: STA ($15,X)
  private def indirectXInstruction: Parser[IndirectX] =
   instructionCode ~  "(" ~ (immediate | address)  ~ argSeparator ~ register ~ ")" ^^ {case inst ~ _ ~ arg1 ~ _ ~ arg2 ~ _  => IndirectX(InstructionNode(inst.toUpperCase(), arg1, arg2))} 

  // Ex: STA ($15), Y
  private def indirectYInstruction: Parser[IndirectY] =
    instructionCode ~ "(" ~ (immediate | address) ~  ")" ~ argSeparator ~ register ^^ {case inst ~ _ ~ arg1 ~ _ ~ _ ~ arg2 => IndirectY(InstructionNode(inst.toUpperCase(), arg1, arg2))}

  private def instruction: Parser[InstructionNode] = twoAryInstruction | oneAryInstruction | zeroAryInstruction

  private def directive: Parser[ASTNode] = labelDefinition | indirectXInstruction | indirectYInstruction |  indirectInstruction | relativeInstruction | instruction

  private def segment: Parser[Segment] = segmentName ~ "{" ~ rep[ASTNode](directive) ~ "}" ^^ {case seg  ~ _ ~ nodes ~ _  => Segment(seg.toUpperCase(), nodes)}

  private def asm: Parser[RootNode] = rep[ASTNode](macros | segment | directive) ^^ { nodes => RootNode(nodes) }


  private def stripOneLineComments(code: String): String =
    code.lines.toStream
      .map((line: String) => line.takeWhile((char) => char.toString() != Lang.ONE_LINE_COMMENT))
      .foldLeft("")((acc: String, line: String) => acc.concat(line))
  

  private def stripMultiLineComments(code: String): String = {
    var input: String = code
    var stripped: String = ""
    var opened: Int = 0

    while(!input.isEmpty()){
      if(input.startsWith(Lang.MULTI_LINE_COMMENT_OPEN)){
        input = input.drop(Lang.MULTI_LINE_COMMENT_OPEN.size)
        opened = opened + 1
      }else if(input.startsWith(Lang.MULTI_LINE_COMMENT_CLOSE)){
        input = input.drop(Lang.MULTI_LINE_COMMENT_CLOSE.size)
        opened = Math.max(opened - 1, 0)
      } else if(opened == 0){
        stripped = stripped.concat(input.head.toString())
        input = input.tail
      } else {
        input = input.tail
      }
    }

    stripped
  }

  def runParser(code: String): ParseResult[RootNode] = {
    // Remove comments
    val stripped: String = stripMultiLineComments(stripOneLineComments(code))
    parse(asm, stripped)
  }
}
