import zio._
import zio.console._
import zio.test._
import zio.test.assertM
import zio.test.Assertion._
import zio.test.environment._
import Expression._
import Type._
import LiteralType._
import ValueType._
import Literal._
import ContextElement._
import TestCommonExpressions._
import CommonTestFunctions.toZIO

object CompilerTest extends DefaultRunnableSpec {

  def spec = suite("CompilerTest")(
    testM("test") {
      // val a: Eff[Int] = ZIO.succeed(1)
      assertM(toZIO(Compiler.stubCompiler.compile.as(1)))(equalTo(1))
      // assertM(toZIO(a))(equalTo(1))
    }
  )
}
