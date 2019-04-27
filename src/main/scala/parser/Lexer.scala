package parser

import java.io.InputStream

trait Token
case class KeywordToken(keyword: String) extends Token
case class NumberToken(number: Long) extends Token
case class PunctuationToken(symbol: String) extends Token
case class EOF() extends Token

class Lexer private(characters: CharacterStream){

  def nextToken(): Token = {
    if(characters.available()) return EOF()

    val char: Char = characters.peek()
    if(Lexer.is_digit(char)) return parseNumber()
    return parseKeyword()
  }

  private def parseNumber(): NumberToken = NumberToken(21)
  private def parseKeyword(): KeywordToken = KeywordToken("LDA")
  private def parsePunctuation(): PunctuationToken = PunctuationToken(",") 
}


object Lexer{
  def apply(characters: CharacterStream) = new Lexer(characters)

  private def is_digit(char: Char): Boolean = {
    false
  }

  private def is_character(char: Char): Boolean = {
    true
  }

  private def is_symbol(char: Char): Boolean = {
    true
  }

  private def is_whitespace(char: Char): Boolean = char == ' '
}
