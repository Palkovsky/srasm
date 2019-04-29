package parser

import java.io.InputStream

sealed trait Token {
  private var startPos: CursorPos = null
  private var endPos: CursorPos = null

  def start(): CursorPos = startPos
  def start(pos: CursorPos): Token = {
    startPos = pos
    this
  }

  def end(): CursorPos = endPos
  def end(pos: CursorPos): Token = {
    endPos = pos
    this
  }
}
case class StringToken(string: String) extends Token
case class SpecialToken(value: String) extends Token
case class EOF() extends Token

final case class LexerException(private val message: String = "") extends Exception(message)


class Lexer private(charStream: CharacterStream){

  def tokenize(): Seq[Token] = {
    var tokens: List[Token] = List()
    while(available()) {
      val token: Token = nextToken()
      if(token != SpecialToken(" ") && token != SpecialToken("\n")){
        tokens = token :: tokens
      }
    }

    tokens = nextToken() :: tokens
    tokens.reverse
  }

  def nextToken(): Token = {
    if(!charStream.available()) return EOF()

    val startPos: CursorPos = charStream.cursorPos()
    val token: Token = {
      if(Lexer.isSpecial(charStream)) parseSpecial()
      else if(Lexer.isString(charStream)) parseString()
      else {
        throw new LexerException(s"Unrecognizable character '${charStream.peek(10)}...' at ${charStream.cursorPos}")
      }
    }
    val endPos: CursorPos = charStream.cursorPos()

    token.start(startPos).end(endPos)
  }

  def available(): Boolean = charStream.available()

  private def parseString(): StringToken = {
    var string: String = ""
    while(!Lexer.isSpecial(charStream)){
      string = string + charStream.next()
    }
    StringToken(string)
  }

  private def parseSpecial(): SpecialToken = {
    if(Lexer.SPECIAL_CHARACTERS.contains(charStream.peek(2)))
      SpecialToken(charStream.next(2))
    else
      SpecialToken(charStream.next(1))
  }
}

object Lexer{

  private val SPECIAL_CHARACTERS = Array(
    "<=",
    ",", "{", "}", "[", "]",
    " ", "\n", "\r\n"
  )

  def apply(charStream: CharacterStream) = new Lexer(charStream)

  private def isString(charStream: CharacterStream): Boolean = {
    val char: Char = charStream.peek()
    char.toInt <= 127
  }

  private def isSpecial(charStream: CharacterStream): Boolean = {
    SPECIAL_CHARACTERS.contains(charStream.peek(2)) ||
    SPECIAL_CHARACTERS.contains(charStream.peek(1))
  }
}
