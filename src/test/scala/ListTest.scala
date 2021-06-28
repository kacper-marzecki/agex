import zio.*
import zio.console.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.environment.*
import Expression.*
import Type.*
import LiteralType.*
import Literal.*
import ContextElement.*
import TestCommonExpressions.*
import CommonTestFunctions.runSynth

object ListTest extends DefaultRunnableSpec {

  def spec = suite("ListTest")(
    testM("works for an emptyTuple type") {
      val expr = ETuple(Nil)
      assertM(runSynth(expr))(
        equalTo(TTuple(Nil))
      )
    }
  )
}
