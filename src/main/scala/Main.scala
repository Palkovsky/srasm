import java.nio.file.{ Files, Paths }
import parser._
import compiler._

object Main extends App {
  if(args.length != 1){
    println("ERROR!! Usage: srasm [file]")
    System.exit(0)
  }

  val path: String = args(0)
  val encoded: Array[Byte] = Files.readAllBytes(Paths.get(path));
  val str: String = new String(encoded);

  val ast: RootNode = ASMParser.runParser(str).get

  println("=========== PREPROCESSED ===========")
  val pair = Preprocessor(ast.nodes).run()
  pair._1.foreach((item) => println(s"${item}, SEGMENT: ${item.segment}"))
  println(pair._2)

  println("Compilation results:")
  val compiler = Compiler(pair)
  val bytecode = compiler.compile()
  compiler.printBytecode()
}
