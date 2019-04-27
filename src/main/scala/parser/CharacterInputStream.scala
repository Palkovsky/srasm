package parser

import java.io.InputStream

class CharacterStream(is: InputStream){

  private var lineNum: Int = 1
  private var columnNum: Int = 1

  def next(): Char = {
    val char: Char = is.read().toChar
    if(char == '\n' || (char == '\r' && peek() == '\n')){
      columnNum = 1
      lineNum += 1
    } else {
      columnNum += 1
    }
    char
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
}
