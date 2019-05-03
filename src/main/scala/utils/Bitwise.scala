package utils

import spire.math.{UByte, UShort}

object Bitwise {

  def word(lower: UByte, upper: UByte): UShort = (UShort(upper.toInt) << 8) + UShort(lower.toInt)

  def upper(word: UShort): UByte = UByte(((word & UShort(0xFF00)) >> 8).toInt)

  def lower(word: UShort): UByte = UByte((word & UShort(0x00FF)).toInt)
}
