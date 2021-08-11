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
import CommonTestFunctions.runSynth
import TMapping.Required

object MapTest extends DefaultRunnableSpec {
  def spec =
    suite("MapTest")(
      testM("basic synthesis") {
        val expr = EMap(
          List(
            (ELiteral(LInt(1)), ELiteral(LString("1"))),
            (ELiteral(LInt(2)), ELiteral(LString("2")))
          )
        )
        assertM(runSynth(expr))(
          equalTo(
            TMap(
              List(
                Required(TValue(VTInt(1)), TValue(VTString("1"))),
                Required(TValue(VTInt(2)), TValue(VTString("2")))
              )
            )
          )
        )
      }
    )
}
