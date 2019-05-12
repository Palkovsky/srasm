package compiler

import scala.collection.mutable
import spire.math.UByte

object AddressingMode extends Enumeration {
  type AddressingMode = Value
  val IMPL, ACC, IMM, ZP, ZP_X, ZP_Y, REL, ABS, ABS_X, ABS_Y, INDIRECT, INDIRECT_X, INDIRECT_Y = Value

  /*
   *  Gives required arugment size of each addresing mode(in bytes).
   */
  def getSize(mode: AddressingMode): Int = mode match {
    case IMPL => 0
    case ACC => 0
    case IMM => 1
    case ZP => 1
    case ZP_X => 1
    case ZP_Y => 1
    case REL => 1
    case ABS => 2
    case ABS_X => 2
    case ABS_Y => 2
    case INDIRECT => 2
    case INDIRECT_X => 1
    case INDIRECT_Y => 1
  }
}

import compiler.AddressingMode._

case class Instruction(name: String, code: UByte, addressing: AddressingMode){
  def getSize(): Int = AddressingMode.getSize(addressing) + 1
}
class InstructionFamily private(val name: String){
  private val instructions: mutable.Set[Instruction] = mutable.HashSet()

  def add(code: Int, addressing: AddressingMode): InstructionFamily = {
    instructions.add(Instruction(name, UByte(code), addressing))
    this
  }

  def get(mode: AddressingMode): Option[Instruction] =
    instructions.filter((inst) => inst.addressing == mode).headOption

  def get(byte: UByte): Option[Instruction] =
    instructions.filter(inst => inst.code == byte).headOption

  def codes(): mutable.Set[UByte] =
    instructions collect {
      case inst: Instruction => inst.code
    }
}

object InstructionFamily{
  def apply(name: String) = new InstructionFamily(name)
}

object InstructionSet {

  def fetch(name: String, mode: AddressingMode): Option[Instruction] =
    lookup.get(name).flatMap((family) => family.get(mode))

  def decode(byte: UByte): Option[Instruction] = {
    for(family <- instructions){
      if(family.codes().contains(byte)) return family.get(byte)
    }
    None
  }

  val instructions: Array[InstructionFamily] = Array(
    // Arithmetic
    InstructionFamily("ADC")
      .add(0x69, IMM)
      .add(0x65, ZP)
      .add(0x75, ZP_X)
      .add(0x6D, ABS)
      .add(0x7D, ABS_X)
      .add(0x79, ABS_Y)
      .add(0x61, INDIRECT_X)
      .add(0x71, INDIRECT_Y),
    InstructionFamily("SBC")
      .add(0xE9, IMM)
      .add(0xE5, ZP)
      .add(0xF5, ZP_X)
      .add(0xED, ABS)
      .add(0xFD, ABS_X)
      .add(0xF9, ABS_Y)
      .add(0xE1, INDIRECT_X)
      .add(0xF1, INDIRECT_Y),

    // Logic functions
    InstructionFamily("AND")
      .add(0x29, IMM)
      .add(0x25, ZP)
      .add(0x35, ZP_X)
      .add(0x2D, ABS)
      .add(0x3D, ABS_X)
      .add(0x39, ABS_Y)
      .add(0x21, INDIRECT_X)
      .add(0x31, INDIRECT_Y),
    InstructionFamily("EOR")
      .add(0x49, IMM)
      .add(0x45, ZP)
      .add(0x55, ZP_X)
      .add(0x4D, ABS)
      .add(0x5D, ABS_X)
      .add(0x59, ABS_Y)
      .add(0x41, INDIRECT_X)
      .add(0x51, INDIRECT_Y),

    InstructionFamily("ORA")
      .add(0x09, IMM)
      .add(0x05, ZP)
      .add(0x15, ZP_X)
      .add(0x0D, ABS)
      .add(0x1D, ABS_X)
      .add(0x19, ABS_Y)
      .add(0x01, INDIRECT_X)
      .add(0x11, INDIRECT_Y),
    InstructionFamily("BIT")
      .add(0x24, ZP)
      .add(0x2C, ABS),

    // Shifts
    InstructionFamily("ASL")
      .add(0x0A, ACC)
      .add(0x06, ZP)
      .add(0x16, ZP_X)
      .add(0x0E, ABS)
      .add(0x1E, ABS_X),

    InstructionFamily("LSR")
      .add(0x4A, ACC)
      .add(0x46, ZP)
      .add(0x56, ZP_X)
      .add(0x4E, ABS)
      .add(0x5E, ABS_X),    

    // Rotations
    InstructionFamily("ROL")
      .add(0x2A, ACC)
      .add(0x26, ZP)
      .add(0x36, ZP_X)
      .add(0x2E, ABS)
      .add(0x3E, ABS_X),
    InstructionFamily("ROR")
      .add(0x6A, ACC)
      .add(0x66, ZP)
      .add(0x76, ZP_X)
      .add(0x6E, ABS)
      .add(0x7E, ABS_X),

    // Increments
    InstructionFamily("INC")
      .add(0xE6, ZP)
      .add(0xF6, ZP_X)
      .add(0xEE, ABS)
      .add(0xFE, ABS_X),
    InstructionFamily("INX").add(0xE8, IMPL),
    InstructionFamily("INY").add(0xC8, IMPL),

    // Decrements
    InstructionFamily("DEC")
      .add(0xC6, ZP)
      .add(0xD6, ZP_X)
      .add(0xCE, ABS)
      .add(0xDE, ABS_X),
    InstructionFamily("DEX").add(0xCA, IMPL),
    InstructionFamily("DEY").add(0x88, IMPL),

    // Comparsion
    InstructionFamily("CMP")
      .add(0xC9, IMM)
      .add(0xC5, ZP)
      .add(0xD5, ZP_X)
      .add(0xCD, ABS)
      .add(0xDD, ABS_X)
      .add(0xD9, ABS_Y)
      .add(0xC1, INDIRECT_X)
      .add(0xD1, INDIRECT_Y),

    InstructionFamily("CPX")
      .add(0xE0, IMM)
      .add(0xE4, ZP)
      .add(0xEC, ABS),

    InstructionFamily("CPY")
      .add(0xC0, IMM)
      .add(0xC4, ZP)
      .add(0xCC, ABS),

    // Branching
    InstructionFamily("BCC").add(0x90, REL),
    InstructionFamily("BCS").add(0xB0, REL),
    InstructionFamily("BEQ").add(0xF0, REL),
    InstructionFamily("BMI").add(0x30, REL),
    InstructionFamily("BNE").add(0xD0, REL),
    InstructionFamily("BPL").add(0x10, REL),
    InstructionFamily("BVC").add(0x50, REL),
    InstructionFamily("BVS").add(0x70, REL),
    InstructionFamily("JMP")
      .add(0x4C, ABS)
      .add(0x6C, INDIRECT),


    // Subroutines/interrupts
    InstructionFamily("BRK").add(0x00, IMPL),
    InstructionFamily("JSR").add(0x20, ABS),
    InstructionFamily("RTI").add(0x40, IMPL),
    InstructionFamily("RTS").add(0x60, IMPL),


    // Flag related
    InstructionFamily("CLS").add(0x18, IMPL),
    InstructionFamily("CLD").add(0xD8, IMPL),
    InstructionFamily("CLI").add(0x58, IMPL),
    InstructionFamily("CLV").add(0xB8, IMPL),
    InstructionFamily("SEC").add(0x38, IMPL),
    InstructionFamily("SED").add(0xF8, IMPL),
    InstructionFamily("SEI").add(0x78, IMPL),    

    // Stack related
    InstructionFamily("PHA").add(0x48, IMPL),
    InstructionFamily("PHP").add(0x08, IMPL),
    InstructionFamily("PLA").add(0x68, IMPL),

    // Register transfers
    InstructionFamily("TAX").add(0xAA, IMPL),
    InstructionFamily("TAY").add(0xA8, IMPL),
    InstructionFamily("TSX").add(0xBA, IMPL),
    InstructionFamily("TXA").add(0x8A, IMPL),
    InstructionFamily("TXS").add(0x9A, IMPL),
    InstructionFamily("TYA").add(0x98, IMPL),

    // NOP
    InstructionFamily("NOP").add(0xEA, IMPL),

    // Loading data
    InstructionFamily("LDA")
      .add(0xA9, IMM)
      .add(0xA5, ZP)
      .add(0xB5, ZP_X)
      .add(0xAD, ABS)
      .add(0xBD, ABS_X)
      .add(0xB9, ABS_Y)
      .add(0xA1, INDIRECT_X)
      .add(0xB1, INDIRECT_Y),
    InstructionFamily("LDX")
      .add(0xA2, IMM)
      .add(0xA6, ZP)
      .add(0xB6, ZP_Y)
      .add(0xAE, ABS)
      .add(0xBE, ABS_Y),
    InstructionFamily("LDY")
      .add(0xA0, IMM)
      .add(0xA4, ZP)
      .add(0xB4, ZP_X)
      .add(0xAC, ABS)
      .add(0xBC, ABS_X),

    // Storing data
    InstructionFamily("STA")
      .add(0x85, ZP)
      .add(0x95, ZP_X)
      .add(0x8D, ABS)
      .add(0x9D, ABS_X)
      .add(0x99, ABS_Y)
      .add(0x81, INDIRECT_X)
      .add(0x91, INDIRECT_Y),
    InstructionFamily("STX")
      .add(0x86, ZP)
      .add(0x96, ZP_Y)
      .add(0x8E, ABS),
    InstructionFamily("STY")
      .add(0x84, ZP)
      .add(0x94, ZP_X)
      .add(0x8C, ABS)
  )

  val instructionsStr: Array[String] = instructions.map((family) => family.name)

  private val lookup: Map[String, InstructionFamily] =
    instructions.map((family) => (family.name, family)).toMap

}
