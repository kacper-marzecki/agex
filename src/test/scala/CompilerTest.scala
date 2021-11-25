import zio.*
import zio.console.*
import zio.test.*
import zio.test.assertM
import zio.test.Assertion.*
import zio.test.environment.*
import Expression.*
import Type.*
import LiteralType.*
import ValueType.*
import Literal.*
import ContextElement.*
import TestCommonExpressions.*
import CommonTestFunctions.toZIO

object CompilerTest extends DefaultRunnableSpec {

  def spec = suite("ContextSpec")(
    testM("test") {
      // val a: Eff[Int] = ZIO.succeed(1)
      assertM(toZIO(Compiler.stubCompiler.compile("as").as(1)))(equalTo(1))
      // assertM(toZIO(a))(equalTo(1))
    }
  )
}
