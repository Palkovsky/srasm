package compiler

import parser._
import AddressingMode._
import SegmentType._
import scala.collection.mutable

object Preprocessor{
  def apply(ast: List[ASTNode]) = new Preprocessor(ast)
}

/*
 * Preprocessor
 * Class responsible for converting AST into format from which it'll be
 * easy to compile into bytes.
 * It also perfoms checks for using wrong addressing modes.
 */
class Preprocessor private(sequence: List[ASTNode]) {

  /*
   * ASTconverter.State contains informations used in traversing AST, such
   * as label definition positions. 
   */
  private class State(var segment: SegmentType, val labels: mutable.Map[String, Int], val macros: Map[String, Int], var result: List[Compilable])
  private object State {
    def apply(
      segment: SegmentType,
      labels: mutable.Map[String, Int],
      macros: Map[String, Int],
      result: List[Compilable] = List()) = new State(segment, labels, macros, result)
  }

  /*
   * runPreprocessor()
   * Translates AST into sequence of Compilable instructions.
   * It also remembers indexes of label definition.
   */
  def run(): (List[Compilable], mutable.Map[String, Int]) = {
    /*
     *  Sort sequence, so that:
     *  Macro definitions
     *  <NONE> segment instructions
     *  <CODE> segment
     *  <NMI> segment
     *  <IRQ> segment
     *  <DATA> segment
     */
    val sorted: List[ASTNode] = sequence.sortWith((a, b) => (a, b) match {
      case (Segment(s1, _), Segment(s2, _)) =>
        (SegmentType.fromString(s1) compare SegmentType.fromString(s2)) < 0
      case(_:MacroNumberDef, _) => true
      case (_, _:Segment) => true
      case (_:Segment, _) => false
      case _ => false
    })

    /*
     * Extract all macro definitions.
     */
    val macros: Map[String, Int] = sorted
      .takeWhile((node) => node.isInstanceOf[MacroNumberDef])
      .map((node) => node.asInstanceOf[MacroNumberDef])
      .map((macroDef) => (macroDef.label.value, macroDef.number.value.toInt)).toMap


    // Remove macro defs from begining
    val stripped: List[ASTNode] = sorted.dropWhile((node) => node.isInstanceOf[MacroNumberDef])

    val state: State = State(NONE, mutable.Map(), macros)
    toCompilable(stripped, state)
    (state.result, state.labels)
  }


  private def toCompilable(nodes: List[ASTNode], state: State): Unit = {
    var rest: List[ASTNode] = nodes

    while(!rest.isEmpty){

      val toadd_rest: (List[Compilable], List[ASTNode]) =  rest match {
        // Flatten segment blocks
        case Segment(segName, nodes) :: tail => {
           // Add segment-label
          state.labels += (segName -> state.result.length)

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
            CompilableDirective(dir, ShortArg(value.toInt))
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
       case InstructionNode(inst, Immediate(Number(value)), null) :: tail if isByte(value) => {
         val instruction: CompilableInst =
           CompilableInst(fetchInstruction(inst, IMM), arg = ByteArg(value.toInt))
         (List(instruction), tail)
       }

       // IMM instruction(label)
       case InstructionNode(inst, Immediate(Label(label)), null) :: tail  => {
         val instruction: CompilableInst =
           CompilableInst(fetchInstruction(inst, IMM), arg = generateArg(label, state.macros))
         (List(instruction), tail)
       }

       // ZP instruction(number)
       case InstructionNode(inst, Number(value), null) :: tail if isByte(value) => {
         val instruction: CompilableInst =
           CompilableInst(fetchInstruction(inst, ZP), arg = ByteArg(value.toInt))
         (List(instruction), tail)
       }

       // ZP_X instruction(number)
       case InstructionNode(inst, Number(value), Register("X")) :: tail if isByte(value) => {
         val instruction: CompilableInst =
           CompilableInst(fetchInstruction(inst, ZP_X), arg = ByteArg(value.toInt))
         (List(instruction), tail)
       }

       // ZP_Y instruction(number)
       case InstructionNode(inst, Number(value), Register("Y")) :: tail if isByte(value) => {
         val instruction: CompilableInst =
           CompilableInst(fetchInstruction(inst, ZP_Y), arg = ByteArg(value.toInt))
         (List(instruction), tail)
       }

       // REL instruction(offset)
       case Relative(InstructionNode(inst, Number(value), null)) :: tail  if isByte(value) => {
         val instruction: CompilableInst =
           CompilableInst(fetchInstruction(inst, REL), arg = ByteArg(value.toInt))
         (List(instruction), tail)
       }

       // REL instruction(label)
       case Relative(InstructionNode(inst, Label(label), null)) :: tail => {
         val instruction: CompilableInst =
           CompilableInst(fetchInstruction(inst, REL), arg = generateArg(label, state.macros))
         (List(instruction), tail)
       }

       // ABS instruction(number)
       case InstructionNode(inst, Number(value), null) :: tail if isShort(value) => {
         val instruction: CompilableInst =
           CompilableInst(fetchInstruction(inst, ABS), arg = ShortArg(value.toInt))
         (List(instruction), tail)
       }

       // ABS instruction(label)
       case InstructionNode(inst, Label(label), null) :: tail  => {
         val instruction: CompilableInst =
           CompilableInst(fetchInstruction(inst, ABS), arg = generateArg(label, state.macros))
         (List(instruction), tail)
       }

       // ABS_X instruction(number)
       case InstructionNode(inst, Number(value), Register("X")) :: tail if isShort(value) => {
         val instruction: CompilableInst =
           CompilableInst(fetchInstruction(inst, ABS_X), arg = ShortArg(value.toInt))
         (List(instruction), tail)
       }

       // ABS_X instruction(label)
       case InstructionNode(inst, Label(label), Register("X")) :: tail  => {
         val instruction: CompilableInst =
           CompilableInst(fetchInstruction(inst, ABS_X), arg = generateArg(label, state.macros))
         (List(instruction), tail)
       }

       // ABS_Y instruction(number)
       case InstructionNode(inst, Number(value), Register("Y")) :: tail if isShort(value) => {
         val instruction: CompilableInst =
           CompilableInst(fetchInstruction(inst, ABS_Y), arg = ShortArg(value.toInt))
         (List(instruction), tail)
       }

       // ABS_Y instruction(label)
       case InstructionNode(inst, Label(label), Register("Y")) :: tail => {
         val instruction: CompilableInst = CompilableInst(fetchInstruction(inst, ABS_Y), arg = generateArg(label, state.macros))
         (List(instruction), tail)
       }

       // INDIRECT instruction (number)
       case Indirect(InstructionNode(inst, Number(value), null)) :: tail  => {
         val instruction: CompilableInst =
           CompilableInst(fetchInstruction(inst, INDIRECT), arg = ByteArg(value.toInt))
         (List(instruction), tail)
       }

       // INDIRECT instruction (label)
       case Indirect(InstructionNode(inst, Label(label), null)) :: tail => {
         val instruction: CompilableInst =
           CompilableInst(fetchInstruction(inst, INDIRECT), arg = generateArg(label, state.macros))
         (List(instruction), tail)
       }

       // INDIRECT_X instruction (number)
       case IndirectX(InstructionNode(inst, Number(value), Register("X"))) :: tail  if isByte(value) => {
         val instruction: CompilableInst =
           CompilableInst(fetchInstruction(inst, INDIRECT_X), arg = ByteArg(value.toInt))
         (List(instruction), tail)
       }

       // INDIRECT_X instruction (label)
       case IndirectX(InstructionNode(inst, Label(label), Register("X"))) :: tail  => {
         val instruction: CompilableInst =
           CompilableInst(fetchInstruction(inst, INDIRECT_X), arg = generateArg(label, state.macros))
         (List(instruction), tail)
       }

       // INDIRECT_Y instruction (number)
       case IndirectY(InstructionNode(inst, Number(value), Register("Y"))) :: tail  if isByte(value) => {
         val instruction: CompilableInst =
           CompilableInst(fetchInstruction(inst, INDIRECT_Y), arg = ByteArg(value.toInt))
         (List(instruction), tail)
       }

       // INDIRECT_Y instruction (label)
       case IndirectY(InstructionNode(inst, Label(label), Register("Y"))) :: tail  => {
         val instruction: CompilableInst =
           CompilableInst(fetchInstruction(inst, INDIRECT_Y), arg = generateArg(label, state.macros))
         (List(instruction), tail)
       }

        case unsupported :: tail =>
          throw new PreprocessorError(s"Unprocessable node: '${unsupported}'.")
    }

      // Update segment information
      toadd_rest._1.foreach((item) => item.segment = state.segment)

      // Append to list
      state.result = state.result ++ toadd_rest._1
      rest = toadd_rest._2

    }
  }

  /*
   * fetchInstruction()
   * Tries to resolve instruction with given addressingmode.
   * if there's none it throws an error.
   */
  private def fetchInstruction(inst: String, mode: AddressingMode): Instruction = InstructionSet.fetch(inst, mode) match {
    case Some(instruction) => instruction
    case None => throw new PreprocessorError(s"Unrecognizable instruction: '${inst}' with '${mode.toString()}' addressing.")
  }

  private def generateArg(label: String, macros: Map[String, Int]): Argument = {
    macros.get(label) match {
      case Some(number) if isByte(number) => ByteArg(number)
      case Some(number) if isShort(number) => ShortArg(number)
      case _ => LabelArg(label)
    }
  }

  // Helpers for validaiting if is valid Zero Page/Absolute
  private def isByte(num: Long): Boolean = num <= 255
  private def isShort(num: Long): Boolean = num <= 0xFFFF
}

