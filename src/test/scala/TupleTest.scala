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
    EFunction(
      List("a"),
      ETuple(List(EVariable("a"), EVariable("a"), ELiteral(LBool(true))))
    ),
    TQuantification(
      "a",
      TFunction(
        List(TVariable("a")),
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
            EFunctionApplication(
              EVariable("function"),
              List(ELiteral(LInt(1)))
            ),
            EFunctionApplication(
              EVariable("function"),
              List(ELiteral(LString("1")))
            )
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
    },
    testM("gives nice error message on tuple length mismatch") {
      val exp = EFunctionApplication(
        EAnnotation(
          EFunction(List("inputTuple"), EVariable("inputTuple")),
          TFunction(
            List(TTuple(List(TLiteral(LTInt)))),
            TTuple(List(TLiteral(LTInt)))
          )
        ),
        List(ETuple(List(ELiteral(LInt(1)), ELiteral(LInt(1)))))
      )
      assertM(runSynth(exp).flip)(
        equalTo(
          AppError.TupleSizesDontMatch(
            TTuple(List(TLiteral(LTInt), TLiteral(LTInt))),
            TTuple(List(TLiteral(LTInt)))
          )
        )
      )
    }
  )
}
