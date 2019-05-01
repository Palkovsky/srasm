import java.nio.file.{ Files, Paths }

object Main extends App {

  if(args.length != 1){
    println("ERROR!! Usage: srasm [file]")
    System.exit(0)
  }

  val path: String = args(0)
  val encoded: Array[Byte] = Files.readAllBytes(Paths.get(path));
  val str: String = new String(encoded);

  val ast: RootNode = ASMParser.runParser(str).get
  ast.nodes.foreach(println)
}
