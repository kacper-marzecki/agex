import zio._
import zio.console._
import zio.test._
import zio.test.Assertion._
import zio.test.environment._
import Expression._
import Type._
import LiteralType._
import ValueType._
import Literal._
import ContextElement._
import TestCommonExpressions._
import CommonTestFunctions._
import javax.lang.model.`type`.TypeMirror
import cats.data.NonEmptyListInstances
import Typer._
object ValueTypeTest extends DefaultRunnableSpec {
  val stringLiteral = ELiteral(LString("a string"))

  def spec = suite("ValueTypeTest")(
    testM("a string literal synthesizes to its value type ") {
      assertM(runSynth(stringLiteral))(
        equalTo(TValue(VTString("a string")))
      )
    },
    testM("a char literal synthesizes to its value type ") {
      assertM(runSynth(ELiteral(LChar('C'))))(
        equalTo(TValue(VTChar('C')))
      )
    },
    testM("a int literal synthesizes to its value type ") {
      assertM(runSynth(ELiteral(LInt(123))))(
        equalTo(TValue(VTInt(123)))
      )
    },
    testM("a boolean literal synthesizes to its value type ") {
      assertM(runSynth(ELiteral(LBool(false))))(
        equalTo(TValue(VTBool(false)))
      )
    },
    testM("an atom literal synthesizes to its value type ") {
      assertM(runSynth(ELiteral(LAtom("atom"))))(
        equalTo(TValue(VTAtom("atom")))
      )
    },
    testM("checks against its literal type") {
      assertM(
        checksAgainst(Context(), stringLiteral, TLiteral(LTString))
          .provideSomeLayer[zio.ZEnv](CompilerState.live)
      )(
        Assertion.anything
      )
    }
  )
}
