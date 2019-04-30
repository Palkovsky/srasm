package parser

import java.io.InputStream

case class CursorPos(line: Int, column: Int){
  override def toString(): String = s"Line: ${line}, Column: ${column}"
}

class CharacterStream(is: InputStream){

  private var lineNum: Int = 1
  private var columnNum: Int = 1

  def next(): Char = {
    val char: Char = is.read().toChar
    if(peek(1) == Lang.NEWLINE || peek(2) == Lang.CRLF){
      columnNum = 1
      lineNum += 1
    } else {
      columnNum += 1
    }
    char
  }

  def next(n: Int): String = {
    val chars: Array[Char] = for(_ <- Array(0 until n)) yield next()
    new String(chars)
  }

  def peek(): Char = {
    is.mark(1)
    val char: Char = is.read().toChar
    is.reset()
    char
  }

  def peek(n: Int): String = {
    is.mark(n)
    val bytes: Array[Byte] = new Array[Byte](n)
    is.read(bytes)
    is.reset()
    new String(bytes)
  }

  def available(): Boolean = is.available() > 0

  def line(): Int = lineNum
  def column(): Int = columnNum
  def cursorPos(): CursorPos = CursorPos(line(), column())
}
