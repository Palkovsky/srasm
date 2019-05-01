package parser

import java.io.InputStream

case class CursorPos(line: Int, column: Int){
  override def toString(): String = s"Line: ${line}, Column: ${column}"
}

class CharacterStream(is: InputStream){

  private var lineNum: Int = 1
  private var columnNum: Int = 1

  def next(): Char = {
    if(peek() == '\r'){
      is.read()
      if(peek() == '\n') is.read()
      columnNum = 1
      lineNum += 1
    }else if(peek() == '\n'){
      is.read()
      columnNum = 1
      lineNum += 1
    } else {
      columnNum += 1
    }

    if(is.available() <= 0) return 0x00.toChar
    is.read().toChar
  }

  def next(n: Int): String = {
    var chars: Seq[Char] = for(_ <- 0 until n; if is.available() > 0) yield next()
    //chars = chars.filter((chr) => chr != 0x00)
    new String(chars.toArray)
  }

  def peek(): Char = {
    is.mark(1)
    val char: Char = is.read().toChar
    is.reset()
    char
  }

  def peek(n: Int): String = {
    is.mark(10*n)

    /*
    var i: Int = 0
    var str: String = ""
    while(available() && i < n){
      val chr: Char = is.read().toChar
      if(chr != '\n' && chr != '\r'){
        str = str + chr
        i = i + 1
      }
     }*/
    val str = next(n)
    

    is.reset()
    str
  }

  def available(): Boolean = is.available() > 0

  def line(): Int = lineNum
  def column(): Int = columnNum
  def cursorPos(): CursorPos = CursorPos(line(), column())
}
