package compiler

import SegmentType._
import scala.collection.mutable
import spire.math.{ UByte, UShort }

object Compiler{
  def apply(code: (List[Compilable], mutable.Map[String, Int])) = new Compiler(code._1, code._2)
}

/*
 * Compiler
 * Class responsible for generating bytecode.
 */
class Compiler private(instructions: List[Compilable], labels: mutable.Map[String, Int]){

  var bytes: Array[UByte] = Array()
  var basePtr: UShort = UShort(0x0000)

  // Label references lookup
  // (Int, Int, String) ==> (Position, Size, Label, DerefOrNo)
  var refs: mutable.Set[(Int, Int, String, Boolean)] = mutable.Set()

  // Lookup of labels inside bytecode
  val labelsLookup: mutable.Map[String, Int] = mutable.Map()

  def compile(): Array[UByte] = {
    var bytesWritten: Int = 0

    /*
     *  Initial phase of transforming into bytecode.
     *  If collects info about lables references, for future correcting.
     */
    for((item, i) <- instructions.zipWithIndex){

      // Check if any label points at this index
      // if so, translate it to byte offset
      for(label <- labels.filter(pair => pair._2 == i).keys){
        labelsLookup += (label -> bytesWritten)
      }

      bytesWritten += {
        item match {
          case CompilableDirective(directive, arg) => handleDirective(directive, arg)
          case CompilableInst(inst, arg) => handleInstruction(inst, arg)
        }
      }}

    /*
     * Second phase - Substitution of labels with addresses.
     */
    for((ptr, size, label, withDeref) <- refs){
      val addr: Int = labelsLookup.get(label) match {
        case Some(value) => value
        case _ => 
          throw new CompilationError(s"Undefined label '${label}'!")
      }
      val short: UShort = UShort(addr)

      (withDeref, size) match {
        // Load lower byte of addr
        case (false, 1) =>
          bytes(ptr) = UByte((short & UShort(0x00FF)).toInt)

        // Load lower and upper bye of addr
        case (false, 2) => {
          bytes(ptr)     = UByte((short & UShort(0x00FF)).toInt)
          bytes(ptr + 1) = UByte((short & UShort(0xFF00)).toInt)
        }

        // Load immediate value into that place
        case (true, _) => for(i <- 0 until size){
          val byte: UByte = bytes(addr + i)
          bytes(ptr + i) = byte
        }
      }
    }

    bytes
  }

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
        val low: UByte = UByte((short & UShort(0x00FF)).toInt)
        val hi: UByte = UByte((short & UShort(0xFF00)).toInt)
        instructionBytes(1) = low
        instructionBytes(2) = hi
      }
      case LabelArg(label) => {
        // Not all labels are available, so we put marker in refs lookup
        refs += ((
          bytes.length + 1,
          AddressingMode.getSize(inst.addressing),
          label,
          inst.addressing == AddressingMode.IMM))
      }
    }

    bytes = bytes ++ instructionBytes
    size
  }

  private def handleDirective(name: String, arg: Argument): Int = (name, arg) match {
    case ("ORG", ShortArg(offset)) => {
      basePtr = UShort(offset)
      0
    }
    case ("DB", ShortArg(byte)) => {
      bytes = bytes ++ Array(UByte(byte))
      1
    }
    case ("DUP", ShortArg(count)) => {
      bytes = bytes ++ Array.fill(count)(UByte(0x00))
      count
    }
    case dir => throw new CompilationError(s"Unknown directive: ${dir}.")
  }
}
