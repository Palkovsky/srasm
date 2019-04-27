import java.io.{ BufferedInputStream, FileInputStream, InputStream }
import parser.{CharacterStream}

object Main extends App {

  if(args.length != 1){
    println("ERROR!! Usage: srasm [file]")
    System.exit(0)
  }

  val path: String = args(0)
  val is: InputStream = new BufferedInputStream(new FileInputStream(path))
  val cs: CharacterStream = new CharacterStream(is)

  print(cs.peek(13));
  while(cs.available())
    print(cs.next());

  Console.println(s"Hello world ${path}")
}
