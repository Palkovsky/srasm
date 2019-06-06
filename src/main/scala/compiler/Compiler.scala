package compiler

import SegmentType._
import parser.Lang
import scala.collection.mutable
import spire.math.{ UByte, UShort }
import utils.{ Bitwise }


object Compiler{
  val IVT_PREAMB_SIZE: Int = 10
  val IVT: Array[(String, UShort)] = Array(("NMI", UShort(0xFFFA)), ("IRQ", UShort(0xFFFE)))

  def apply(code: (List[Compilable], mutable.Map[String, Int])) = new Compiler(code._1, code._2)
}

/*
 * Compiler
 * Class responsible for generating bytecode.
 */
class Compiler private(instructions: List[Compilable], labels: mutable.Map[String, Int]){

  private sealed abstract class ReferenceType
  private case class RelativeRef() extends ReferenceType
  private case class ImmediateRef() extends ReferenceType
  private case class AddressRef() extends ReferenceType
  private case class Reference(position: Int, size: Int, label: String, refType: ReferenceType)

  private var bytes: Array[UByte] = Array()
  private var basePtr: UShort = UShort(0x0000)

  // Label references lookup
  private var refs: mutable.Set[Reference] = mutable.Set()

  // Lookup of labels inside bytecode
  private val labelsLookup: mutable.Map[String, Int] = mutable.Map()

  def compile(): Array[UByte] = {
    /*
     * Phase 1 - Detect IRQ/NMI segments. Add proper padding at begining.
     */
    for((ivName, _) <- Compiler.IVT if labels.contains(ivName)){
      bytes = bytes ++ Array.fill(Compiler.IVT_PREAMB_SIZE)(UByte(0x00))
    }

    /*
     *  Phase 2
     *  Transform what's possible into bytecode. Collect info about label referenes.
     */
    for((item, i) <- instructions.zipWithIndex){

      // Check if any label points at this index
      // if so, translate it from indexes to bytes
      for(label <- labels.filter(pair => pair._2 == i).keys){
        labelsLookup += (label -> bytes.length)
      }

      item match {
        case CompilableDirective(directive, arg) => handleDirective(directive, arg)
        case CompilableInst(inst, arg) => handleInstruction(inst, arg)
      }
    }

    /*
     * Phase 3
     * Setup IVT with collected IRQ/NMI address.
     */
    var ivtOff: Int = 0
    for((ivName, addr) <- Compiler.IVT if labelsLookup.contains(ivName)){
      handleIVT(ivName, addr, ivtOff)
      ivtOff += Compiler.IVT_PREAMB_SIZE
    }

    /*
     * Phase 4
     * Substitite label references with addresses.
     */
    for((ptr, size, label, refType) <- refs.map(ref => Reference.unapply(ref).get)){

      // Lookup labels table for address
      val addr: Int = labelsLookup.get(label) match {
        case Some(value) => value
        case _ =>
          throw new CompilationError(s"Undefined label '${label}'!")
      }
      val short: UShort = UShort(addr)

      (refType, size) match {
        // Imediate for labels means that we don't dereference address
        // only copy address itself.
        case (AddressRef(), 1) => {
          bytes(ptr) = UByte((short & UShort(0x00FF)).toInt)
        }
        case (AddressRef(), 2) => {
          val withOffset: UShort = getRelativePosition(short)
          bytes(ptr)     = Bitwise.lower(withOffset)
          bytes(ptr + 1) = Bitwise.upper(withOffset)
        }

         // Calculate byte offset from current position (ptr) to target (addr).
        case (RelativeRef(), _) => {
          val offset: Int = addr - ptr
          if(offset < -128 || offset > 127) throw new CompilationError(s"Unable to branch to ${label}. Offset too big: ${offset}.")
          bytes(ptr) = UByte(offset)
        }

        // Most common - copy address pointed by label
        case (ImmediateRef(), _) => {
          for(i <- 0 until size){
            val byte: UByte = bytes(addr + i)
            bytes(ptr + i) = byte
          }
        }
      }
    }

    bytes
  }

  /*
   * Initialize interupt handler.
   * For IRQ:
   *   LDA #LO IRQ  // 2 bytes
   *   STA $FFFE    // 3 bytes
   *   LDA #HI IRQ  // 2 byres
   *   STA $FFFF    // 3 bytes
   */
  private def handleIVT(name: String, ivtAddr: UShort, offset: Int): Unit = {
    val handlerAddr: UShort =  getRelativePosition(UShort(labelsLookup.get(name).get))
    val code: Array[UByte] = Array(
      // Lower byte
      InstructionSet.fetch("LDA", AddressingMode.IMM).get.code,
      Bitwise.lower(handlerAddr),
      InstructionSet.fetch("STA", AddressingMode.ABS).get.code,
      Bitwise.lower(ivtAddr),
      Bitwise.upper(ivtAddr),

      // Higher byte
      InstructionSet.fetch("LDA", AddressingMode.IMM).get.code,
      Bitwise.upper(handlerAddr),
      InstructionSet.fetch("STA", AddressingMode.ABS).get.code,
      Bitwise.lower(ivtAddr + UShort(1)),
      Bitwise.upper(ivtAddr + UShort(1))
    )

    for(i <- 0 until code.size) bytes(offset + i) = code(i)
  }

  /*
   * handeInstruction()
   * Puts instruction bytes into output.
   * If instruction uses label, it is remembered here.
   */
  private def handleInstruction(inst: Instruction, arg: Argument): Int = {
    val size: Int = inst.getSize
    val instructionBytes: Array[UByte] = Array.fill(size)(UByte(0x00))

    // Instruction code as first byte
    instructionBytes(0) = inst.code

    arg match {
      case null => ()
      case ByteArg(value) => instructionBytes(1) = UByte(value)
      case ShortArg(value) => {
        val short: UShort = UShort(value)
        instructionBytes(1) = Bitwise.lower(short)
        instructionBytes(2) = Bitwise.upper(short)
      }
      case LabelArg(label) => {
        // Not all labels are available, so we put information in refs lookup
        val refType: ReferenceType = inst.addressing match {
          case AddressingMode.REL => RelativeRef()
          case AddressingMode.IMM => ImmediateRef()
          case _ => AddressRef()
        }

        refs += Reference(
          bytes.length + 1,
          AddressingMode.getSize(inst.addressing),
          label,
          refType)
      }
    }

    bytes = bytes ++ instructionBytes
    size
  }

  /*
   * handleDirective()
   * Implements directives logic.
   */
  private def handleDirective(name: String, arg: Argument): Int = (name, arg) match {
    case (Lang.ORG_DIRECTIVE, ShortArg(offset)) => {
      basePtr = UShort(offset)
      0
    }
    case (Lang.DB_DIRECTIVE, ShortArg(byte)) => {
      bytes = bytes ++ Array(UByte(byte))
      1
    }
    case (Lang.DUP_DIRECTIVE, ShortArg(count)) => {
      bytes = bytes ++ Array.fill(count)(UByte(0x00))
      count
    }
    case (Lang.STR_DIRECTIVE, StringArg(value)) => {
      val strBytes: Array[UByte] = value.getBytes.map((byte) => UByte(byte))
      bytes = bytes ++ strBytes
      strBytes.size
    }
    case (Lang.OFF_DIRECTIVE, StringArg(label)) => {
      refs.add(Reference(bytes.length, 2, label, AddressRef()))
      bytes = bytes ++ Array.fill(2)(UByte(0x00))
      2
    }

    case dir => throw new CompilationError(s"Unknown directive: ${dir}.")
  }

  /*
   * Relative address form label resolving.
   */
  private def getRelativePosition(addr: UShort): UShort = basePtr  + addr

    /*
   * printBytecode()
   * Pretty-prints output bytcode. 
   * Should be called after compile()-ing first.
   */
  def printBytecode(): Unit = {
    var shouldDecode: Int = 0
    var position: Int = basePtr.toInt

    for((byte, i) <- bytes.zipWithIndex){

      // Print labels pointg to this point
      for(label <- labelsLookup.filter(pair => pair._2 == i).keys){
        if(Lang.isSegment(label)) println(s"==${label}==")
        else println(s"${label}:")
      }

      if(shouldDecode == 0){
        InstructionSet.decode(byte) match {
          case Some(Instruction(name, size, addressing)) => {
            println(s"0x${position.toHexString}:\t\t0x${byte.toInt.toHexString} (${name}, ${addressing})")
            shouldDecode = AddressingMode.getSize(addressing) + 1
          }
          case _ => println(s"0x${position.toHexString}:\t\t0x${byte.toInt.toHexString}")
        }
      } else{
        println(s"0x${position.toHexString}:\t\t0x${byte.toInt.toHexString}")
      }

      shouldDecode = shouldDecode - 1
      position += 1
    }

    println(s"\nBytesize: ${bytes.length}")
  }
}
