package compiler

import parser._
import AddressingMode._
import SegmentType._
import scala.collection.mutable.Map

/*
 * ASTConverter
 * Class responsible for converting AST into format from which it'll be
 * easy to compile into bytes.
 * 
 * It also perfoms checks for using wrong addressing modes.
 */
class ASTConverter(sequence: List[ASTNode]) {

  /*
   * ASTconverter.State contains informations used in traversing AST, such
   * as label definition positions. 
   */
  private class State(var segment: SegmentType, val labels: Map[String, Int], var result: List[Compilable])
  private object State {
    def apply(segment: SegmentType, labels: Map[String, Int], result: List[Compilable] = List()) = new State(segment, labels, result)
  }

  /*
   * runConverter()
   * Translates AST into sequence of Compilable instructions.
   * It also remembers indexes of label definition.
   */
  def runConverter(): (List[Compilable], Map[String, Int]) = {
    val state: State = State(NONE, Map())
    toCompilable(sequence, state)
    (state.result, state.labels)
  }


  private def toCompilable(nodes: List[ASTNode], state: State): Unit = {
    var rest: List[ASTNode] = nodes

    while(!rest.isEmpty){

      val toadd_rest: (List[Compilable], List[ASTNode]) =  rest match {
        // Flatten segment blocks
        case Segment(segName, nodes) :: tail => {
          val currentSegment: SegmentType = state.segment
          state.segment = SegmentType.fromString(segName)
          toCompilable(nodes, state)
          state.segment = currentSegment
          (List(), tail)
        }

       // Label definition, won't add anything. Just remembers next index.
       case LabelDefinition(label) :: tail => {
          state.labels += (label -> state.result.length)
          (List(), tail)
       }

       // Compiler Directives
        case InstructionNode(dir, Number(value), null) :: tail if Lang.isDirective(dir) => {
          val directive: Compilable =
            CompilableDirective(dir, NumberArg(value))
         (List(directive), tail)
        }        

       // Implied instruction
       case InstructionNode(inst, null, null) :: tail => {
         val instruction: CompilableInst = CompilableInst(fetchInstruction(inst, IMPL))
         (List(instruction), tail)
       }

       // ACC instruction
        case InstructionNode(inst, Register("A"), null) :: tail  => {
          val instruction: CompilableInst = CompilableInst(fetchInstruction(inst, ACC))
         (List(instruction), tail)
        }

       // IMM instruction(number)
       case InstructionNode(inst, Immediate(Number(value)), null) :: tail  => {
         val instruction: CompilableInst =
           CompilableInst(fetchInstruction(inst, IMM), arg = NumberArg(value))
         (List(instruction), tail)
       }

       // IMM instruction(label)
       case InstructionNode(inst, Immediate(Label(label)), null) :: tail  => {
         val instruction: CompilableInst =
           CompilableInst(fetchInstruction(inst, IMM), arg = LabelArg(label))
         (List(instruction), tail)
       }

       // ZP instruction(number)
       case InstructionNode(inst, Number(value), null) :: tail if isByte(value) => {
         val instruction: CompilableInst = CompilableInst(fetchInstruction(inst, ZP), arg = NumberArg(value))
         (List(instruction), tail)
       }       

       // ZP_X instruction(number)
       case InstructionNode(inst, Number(value), Register("X")) :: tail if isByte(value) => {
         val instruction: CompilableInst = CompilableInst(fetchInstruction(inst, ZP_X), arg = NumberArg(value))
         (List(instruction), tail)
       }      

       // ZP_Y instruction(number)
       case InstructionNode(inst, Number(value), Register("Y")) :: tail if isByte(value) => {
         val instruction: CompilableInst = CompilableInst(fetchInstruction(inst, ZP_Y), arg = NumberArg(value))
         (List(instruction), tail)
       }

       // REL instruction(offset)
       case Relative(InstructionNode(inst, Number(value), null)) :: tail  if isByte(value) => {
         val instruction: CompilableInst = CompilableInst(fetchInstruction(inst, REL), arg = NumberArg(value))
         (List(instruction), tail)
       }

       // REL instruction(label)
       case Relative(InstructionNode(inst, Label(label), null)) :: tail => {
         val instruction: CompilableInst = CompilableInst(fetchInstruction(inst, REL), arg = LabelArg(label))
         (List(instruction), tail)
       }          

       // ABS instruction(number)
       case InstructionNode(inst, Number(value), null) :: tail if isShort(value) => {
         val instruction: CompilableInst = CompilableInst(fetchInstruction(inst, ABS), arg = NumberArg(value))
         (List(instruction), tail)
       }

       // ABS instruction(label)
       case InstructionNode(inst, Label(label), null) :: tail  => {
         val instruction: CompilableInst = CompilableInst(fetchInstruction(inst, ABS), arg = LabelArg(label))
         (List(instruction), tail)
       }          

       // ABS_X instruction(number)
       case InstructionNode(inst, Number(value), Register("X")) :: tail if isShort(value) => {
         val instruction: CompilableInst = CompilableInst(fetchInstruction(inst, ABS_X), arg = NumberArg(value))
         (List(instruction), tail)
       }

       // ABS_X instruction(label)
       case InstructionNode(inst, Label(label), Register("X")) :: tail  => {
         val instruction: CompilableInst = CompilableInst(fetchInstruction(inst, ABS_X), arg = LabelArg(label))
         (List(instruction), tail)
       }             

       // ABS_Y instruction(number)
       case InstructionNode(inst, Number(value), Register("Y")) :: tail if isShort(value) => {
         val instruction: CompilableInst = CompilableInst(fetchInstruction(inst, ABS_Y), arg = NumberArg(value))
         (List(instruction), tail)
       }

       // ABS_Y instruction(label)
       case InstructionNode(inst, Label(label), Register("Y")) :: tail => {
         val instruction: CompilableInst = CompilableInst(fetchInstruction(inst, ABS_Y), arg = LabelArg(label))
         (List(instruction), tail)
       }          

       // INDIRECT instruction (number)
       case Indirect(InstructionNode(inst, Number(value), null)) :: tail => {
         val instruction: CompilableInst = CompilableInst(fetchInstruction(inst, INDIRECT), arg = NumberArg(value))
         (List(instruction), tail)
       }

       // INDIRECT instruction (label)
       case Indirect(InstructionNode(inst, Label(label), null)) :: tail => {
         val instruction: CompilableInst = CompilableInst(fetchInstruction(inst, INDIRECT), arg = LabelArg(label))
         (List(instruction), tail)
       } 

       // INDIRECT_X instruction (number)
       case IndirectX(InstructionNode(inst, Number(value), Register("X"))) :: tail  if isByte(value) => {
         val instruction: CompilableInst = CompilableInst(fetchInstruction(inst, INDIRECT_X), arg = NumberArg(value))
         (List(instruction), tail)
       }

       // INDIRECT_X instruction (label)
       case IndirectX(InstructionNode(inst, Label(label), Register("X"))) :: tail  => {
         val instruction: CompilableInst = CompilableInst(fetchInstruction(inst, INDIRECT_X), arg = LabelArg(label))
         (List(instruction), tail)
       }       

       // INDIRECT_Y instruction (number)
       case IndirectY(InstructionNode(inst, Number(value), Register("Y"))) :: tail  if isByte(value) => {
         val instruction: CompilableInst = CompilableInst(fetchInstruction(inst, INDIRECT_Y), arg = NumberArg(value))
         (List(instruction), tail)
       }

       // INDIRECT_Y instruction (label)
       case IndirectY(InstructionNode(inst, Label(label), Register("Y"))) :: tail  => {
         val instruction: CompilableInst = CompilableInst(fetchInstruction(inst, INDIRECT_Y), arg = LabelArg(label))
         (List(instruction), tail)
       }        
    }

      // Update segment information
      toadd_rest._1.foreach((item) => item.segment = state.segment)

      // Append to list
      state.result = state.result ++ toadd_rest._1
      rest = toadd_rest._2

    }
  }

  /*
   * fetchinstruction()
   * Tries to resolve instruction with given addressingmode.
   * if there's none it throws an error.
   */
  private def fetchInstruction(inst: String, mode: AddressingMode): Instruction = InstructionSet.fetch(inst, mode) match {
    case Some(instruction) => instruction
    case None => throw new CompilationError(s"Unrecognizable instruction: '${inst}' with '${mode.toString()}' addressing.")
  }

  // Helpers for validaiting if is valid Zero Page/Absolute
  private def isByte(num: Long): Boolean = num <= 255
  private def isShort(num: Long): Boolean = num <= 0xFFFF
}

