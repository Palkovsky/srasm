import java.nio.file.{ Files, Paths }
import java.io.FileOutputStream
import java.io.BufferedOutputStream
import java.io.File
import scopt.OParser

import parser._
import compiler._

case class Config(
  in: File = new File("."),
  out: File = null,
  verbose: Boolean = true
)

object Main extends App {

  val builder = OParser.builder[Config]
  val argParser = {
    import builder._

    OParser.sequence(
      programName("srasm"),
      head("srasm", "0.1"),

      opt[File]('o', "output")
        .valueName("<out_binary>")
        .action((x,c) => c.copy(out = x))
        .text("file to save resulting binary to"),

      opt[Boolean]('v', "verbose")
        .action((f, c) => c.copy(verbose = f))
        .text("verbose compilation information"),

      arg[File]("<in_file>")
        .action((f, c) => c.copy(in = f))
        .text("file containing code to compile")
    )
  }


  OParser.parse(argParser, args, Config()) match {
  case Some(config) => {

    val code: String = new String(Files.readAllBytes(config.in.toPath()))
    val ast: RootNode = ASMParser.runParser(code) match {
      case ASMParser.Success(root: RootNode, _) => root
      case failure: ASMParser.Failure => {
        println("=========== FAILURE  ===========")
        println(failure.toString())
        System.exit(0)
        null
      }
      case error: ASMParser.Error => {
        println("=========== ERROR  ===========")
        println(error.toString())
        System.exit(0)
        null
      }
    }

    if(config.verbose){
      println("=========== AST  ===========")
      println(ast.nodes)
    }

    val pair = Preprocessor(ast.nodes).run()
    if(config.verbose){
      println("=========== PREPROCESSED ===========")
      pair._1.foreach((item) => println(s"${item}, SEGMENT: ${item.segment}"))
      println(pair._2)
    }

    val compiler = Compiler(pair)
    val bytecode = compiler.compile().map((ubyte) => ubyte.toByte)
    if(config.verbose){
      println("=========== COMPILATION  ===========")
      compiler.printBytecode()
    }

    val output: File = if (config.out == null) new File(config.in.getName() + ".out") else config.out
    val bos = new BufferedOutputStream(new FileOutputStream(output))
    bos.write(bytecode)
    bos.close()

    println(s"Saved to ${output.getName}")
  }
  case _ =>
      System.exit(0)
}
}
