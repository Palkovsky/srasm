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
case class BlockToken(tokens: List[Token]) extends Token
case class DerefToken(value: String) extends Token
case class EOFToken() extends Token

final case class LexerException(private val message: String = "") extends Exception(message)

class Lexer private(charStream: CharacterStream){

  def tokenize(): BlockToken = tokenize(CursorPos(1, 1), 0)

  private def tokenize(blockPos: CursorPos, blockDepth: Int): BlockToken = {
    var tokens: List[Token] = List()

    while(true) {
      val token: Token = nextToken()

      token match {
        // Ignored tokens
        case SpecialToken(Lang.SPACE) => ()
        case SpecialToken(Lang.NEWLINE) => ()
        case SpecialToken(Lang.CRLF) => ()
        case SpecialToken(Lang.CR) => ()

        // Dereference opening
        case SpecialToken(Lang.DEREF_PARENS_OPEN) => {
          val value: Token = nextToken()
          val closing: Token = nextToken()

          if(!value.isInstanceOf[StringToken]){
            throw new LexerException(s"Expecting address at ${value.start()}")
          }

          if(!closing.isInstanceOf[SpecialToken] || closing.asInstanceOf[SpecialToken].value != Lang.DEREF_PARENS_CLOSE){
            throw new LexerException(s"Expecting '${Lang.DEREF_PARENS_CLOSE}' at ${closing.start()}")
          }
 
          tokens = DerefToken(value.asInstanceOf[StringToken].string) :: tokens
        }

        // Block opening
        case SpecialToken(Lang.BLOCK_PARENS_OPEN) => {
          val block = tokenize(token.start(), blockDepth + 1)
          tokens = block :: tokens
        }

        // Block closing/EOF
        case SpecialToken(Lang.BLOCK_PARENS_CLOSE) => {
          if(blockDepth <= 0)
            throw new LexerException(s"Unexpected '${Lang.BLOCK_PARENS_CLOSE}' at ${token.start()}")
          return BlockToken(tokens.reverse)
        }
        case EOFToken() => {
          if(blockDepth > 0)
            throw new LexerException(s"Unexpected EOF. Block not properly enclosed. At ${blockPos}")
          return BlockToken(tokens.reverse)
        }

        case token => tokens = token :: tokens
      }
    }

    throw new LexerException(s"Unexpected EOF. Block not properly enclosed. At ${blockPos}")
  }

   def nextToken(): Token = {
     if(!charStream.available()) 
       return EOFToken()

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

  private def parseString(): StringToken = {
    var string: String = ""
    while(!Lexer.isSpecial(charStream)){
      string = string + charStream.next()
    }
    StringToken(string)
  }

  private def parseSpecial(): SpecialToken = {
    if(Lexer.SPECIAL_CHARACTERS.contains(charStream.peek(2))){
      SpecialToken(charStream.next(2))
    } else {
      SpecialToken(charStream.next(1))
    }
  }
}

object Lexer{

  private val SPECIAL_CHARACTERS: Array[String] = Array(
    Lang.ADDR_ASSIGMENT,
    Lang.DEREF_PARENS_OPEN, Lang.DEREF_PARENS_CLOSE,
    Lang.BLOCK_PARENS_OPEN, Lang.BLOCK_PARENS_CLOSE,
    Lang.DIRECTIVE_PARENS_OPEN, Lang.DIRECTIVE_PARENS_CLOSE,
    Lang.ARG_SEPARATOR,
    Lang.SPACE, Lang.NEWLINE, Lang.CRLF, Lang.CR
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
