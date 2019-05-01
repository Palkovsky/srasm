package parser

import java.lang.{Long, NumberFormatException}

sealed abstract class ASTNode

case class Instruction(inst: String, arg1: Literal = null, arg2: Literal = null) extends ASTNode
case class Block(Instructions: Seq[ASTNode]) extends ASTNode
case class DirectiveCall(name: String, args: Array[String]) extends ASTNode

trait Literal extends ASTNode
case class NumberLiteral(num: Long) extends ASTNode with Literal
case class LabelLiteral(label: String) extends ASTNode with Literal
case class RegisterLiteral(name: String) extends ASTNode with Literal

case class Dereference(addr: NumberLiteral) extends ASTNode
case class BlockAssigment(label: String, dest: Block) extends ASTNode
case class LiteralAssigment(label: String, dest: Literal) extends ASTNode

case class Unknown() extends ASTNode

class Parser() {

  def parse(tokens: List[Token]): List[ASTNode] = parse(tokens, List())

  private def parse(tokens: List[Token], acc: List[ASTNode]): List[ASTNode] = {

    // Stop conditions
    tokens match {
      case List() => return acc.reverse
      case List(EOFToken()) => return acc.reverse
      case _ => ()
    }

    // Actual parsing
    val pair: (ASTNode, List[Token]) = tokens match {

      // Literals
      case StringToken(str) :: tail if isLiteral(str) => {
        (parseLiteral(str), tail)
      }

      // {...}
      case BlockToken(tokens) :: tail => (Block(parse(tokens, List())), tail)

      // [addr]
      case SpecialToken(Lang.DEREF_PARENS_OPEN) :: StringToken(numString) :: SpecialToken(Lang.DEREF_PARENS_CLOSE) :: tail if isNum(numString) => {
        val deref: Dereference = Dereference(parseNum(numString))
        (deref, tail)
      }

      // Block assigment | LABEL <= {...}
      case StringToken(label) :: SpecialToken(Lang.ADDR_ASSIGMENT) :: BlockToken(tokens) :: tail => {
        (BlockAssigment(label, (Block(parse(tokens, List())))), tail)
      }

      // Data assigment | ARR <= 21h, 22h, 23h, 00h
      case StringToken(label) :: SpecialToken(Lang.ADDR_ASSIGMENT) :: tail => {
        val parsed: (Literal, List[Token]) = parseData(tail)
        (LiteralAssigment(label, parsed._1), parsed._2)
      }

      // Directive call(0-ary) | directive()
      case StringToken(directiveName) ::SpecialToken(Lang.DIRECTIVE_PARENS_OPEN) :: SpecialToken(Lang.DIRECTIVE_PARENS_CLOSE) :: tail if Lang.isDirective(directiveName) => {
        (DirectiveCall(directiveName, Array()), tail)
      }
      // Directive call(1-ary) | directive(arg1)
      case StringToken(directiveName) :: SpecialToken(Lang.DIRECTIVE_PARENS_OPEN) :: StringToken(arg1) :: SpecialToken(Lang.DIRECTIVE_PARENS_CLOSE) :: tail if Lang.isDirective(directiveName) => {
        (DirectiveCall(directiveName, Array(arg1)), tail)
      }
/*
      // Two-arg instruction
      case StringToken(inst) :: tail if Lang.isInstruction(inst) =>   {
        (Instruction(inst, parseLiteral(arg1), parseLiteral(arg2)), tail)
      }

      // One-arg instruction
      case StringToken(inst) :: StringToken(arg1) :: tail if Lang.isInstruction(inst) =>   {
        (Instruction(inst, parseLiteral(arg1)), tail)
      }

      // Zero-arg instruction
      case StringToken(inst) :: tail if Lang.isInstruction(inst) => {
        (Instruction(inst), tail)
 }*/

      case _ :: tail => (Unknown(), tail)
    }

    val node = pair._1
    val tail = pair._2

    parse(tail, node :: acc)
  }

  private def isNum(numStr: String): Boolean = {
    val str: String = numStr.toLowerCase().trim()
    val base_chars: (Int, String) = {
      if(str.startsWith("0x")) (16, str.substring(2))
      else if(str.startsWith("0b")) (2, str.substring(2))
      else (10, str)
    }

    try{
      Long.parseLong(base_chars._2, base_chars._1)
    } catch{
      case e: NumberFormatException => return false
    }

    return true
  }

  private def isLiteral(str: String): Boolean =
    Lang.isRegister(str) || isNum(str)

  private def parseNum(numStr: String): NumberLiteral = {
    val str: String = numStr.toLowerCase().trim()
    val base_chars: (Int, String) = {
      if(str.startsWith("0x")) (16, str.substring(2))
      else if(str.startsWith("0b")) (2, str.substring(2))
      else (10, str)
    }
    NumberLiteral(new Long(Long.parseLong(base_chars._2, base_chars._1)))
  }

  private def parseData(tokens: List[Token]): (Literal, List[Token]) = (null, tokens)
  private def parseLiteral(token: String): Literal = {
    if(Lang.isRegister(token)) return RegisterLiteral(token)
    parseNum(token)
  }
}
