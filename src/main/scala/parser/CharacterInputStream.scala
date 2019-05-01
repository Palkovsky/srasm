package parser

import java.io.{ LineNumberReader, FileReader }

case class CursorPos(line: Int, column: Int = 1){
  override def toString(): String = s"Line: ${line}"
}

class CharacterStream(path: String){

  val file: LineNumberReader = new LineNumberReader(new FileReader(path))

  def next(): Char = {
    file.read().toChar
  }

  def next(n: Int): String = {
    val chars: Array[Char] = new Array[Char](n)
    file.read(chars)
    new String(chars)
  }

  def peek(): Char = {
    file.mark(1)
    val char: Char = file.read().toChar
    file.reset()
    char
  }

  def peek(n: Int): String = {
    file.mark(3*n)
    val str = next(n)
    file.reset()
    str
  }

  def available(): Boolean = file.ready()

  def line(): Int = file.getLineNumber()
  def column(): Int = 1
  def cursorPos(): CursorPos = CursorPos(line(), column())
}
