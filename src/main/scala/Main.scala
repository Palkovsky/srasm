import java.io.{ FileReader, LineNumberReader, Reader }
import parser.{CharacterStream, Lexer, Token, Parser}

object Main extends App {

  if(args.length != 1){
    println("ERROR!! Usage: srasm [file]")
    System.exit(0)
  }

  val path: String = args(0)
  val lexer: Lexer = Lexer(new CharacterStream(path))
  val parser: Parser = new Parser()
  val tokens: List[Token] = lexer.tokenize().tokens

  println("TOKENIZED:")
  println(tokens)

  println("======")
  println("PARSED:")
  println(parser.parse(tokens))
}
