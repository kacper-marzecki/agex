import zio.*
import zio.console.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.environment.*
import Expression.*
import Type.*
import LiteralType.*
import ValueType.*
import Literal.*
import ContextElement.*
import TestCommonExpressions.*
import CommonTestFunctions.*
import javax.lang.model.`type`.TypeMirror
import cats.data.NonEmptyListInstances

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
