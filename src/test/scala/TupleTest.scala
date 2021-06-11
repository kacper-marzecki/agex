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

object TupleTest extends DefaultRunnableSpec {
  val aToTupleOfAABool = EAnnotation(
    ELambda(
      "a",
      ETuple(List(EVariable("a"), EVariable("a"), ELiteral(LBool(true))))
    ),
    TQuantification(
      "a",
      TFunction(
        TVariable("a"),
        TTuple(List(TVariable("a"), TVariable("a"), TLiteral(LTBool)))
      )
    )
  )

  def spec = suite("TupleTest")(
    testM("works for an emptyTuple type") {
      val expr = ETuple(Nil)
      assertM(runSynth(expr))(
        equalTo(TTuple(Nil))
      )
    },
    testM("works for a long tuple") {
      val expr =
        ETuple(repeat(List(litInt, litBool, litString), 100))
      assertM(runSynth(expr))(
        equalTo(
          TTuple(
            repeat(
              List(TLiteral(LTInt), TLiteral(LTBool), TLiteral(LTString)),
              100
            )
          )
        )
      )
    },
    testM("generic tuple type ") {
      val exp = ELet(
        "function",
        aToTupleOfAABool,
        ETuple(
          List(
            EApplication(EVariable("function"), ELiteral(LInt(1))),
            EApplication(EVariable("function"), ELiteral(LString("1")))
          )
        )
      )
      assertM(runSynth(exp))(
        equalTo(
          TTuple(
            List(
              TTuple(
                List(TLiteral(LTInt), TLiteral(LTInt), TLiteral(LTBool))
              ),
              TTuple(
                List(TLiteral(LTString), TLiteral(LTString), TLiteral(LTBool))
              )
            )
          )
        )
      )
    }
  )
}
