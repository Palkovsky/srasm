import java.io.{ BufferedInputStream, FileInputStream, InputStream }
import parser.{CharacterStream, Lexer, Token, EOFToken}

object Main extends App {

  if(args.length != 1){
    println("ERROR!! Usage: srasm [file]")
    System.exit(0)
  }

  val path: String = args(0)
  val is: InputStream = new BufferedInputStream(new FileInputStream(path))
  val lexer: Lexer = Lexer(new CharacterStream(is))

  
  var t: Token = lexer.nextToken()
  while(t != EOFToken()) {
    println(t)
    t = lexer.nextToken()
  }


  //println("TOKENIZED:")
  //println(lexer.tokenize().tokens(0))

}
