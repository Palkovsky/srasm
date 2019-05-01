import org.scalatest.{FunSpec, Matchers}

class BaseSpec extends FunSpec with Matchers {
  def parse(code: String): Seq[ASTNode] = ASMParser.runParser(code) match {
    case ASMParser.Success(root: RootNode, _) => root.nodes
    case ASMParser.Failure(msg, _) => fail(msg)
    case ASMParser.Error(msg, _) => fail(msg)
  }
}
