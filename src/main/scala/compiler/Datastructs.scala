package compiler

object SegmentType extends Enumeration {
  type SegmentType = Value
  val NONE, CODE, DATA = Value

  def fromString(segName: String) = segName.toUpperCase() match {
    case "DATA" => DATA
    case "CODE" => CODE
    case _ => NONE
  }
}
import SegmentType._


sealed abstract class Argument
case class NumberArg(value: Long) extends Argument
case class LabelArg(value: String) extends Argument

sealed abstract class Compilable(){
  var segment: SegmentType = NONE
}

case class CompilableInst(inst: Instruction, arg: Argument = null) extends Compilable
case class CompilableDirective(directive: String, arg: Argument = null) extends Compilable

final case class CompilationError(private val message: String = "") extends Exception(message)
