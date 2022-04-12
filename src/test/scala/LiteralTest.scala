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
import CommonTestFunctions.runSynth

object LiteralTest extends DefaultRunnableSpec {
  def spec = suite("LiteralTest")(
    suite("AtomTest")(
      testM("correctly infers type") {
        val expr = ELiteral(LAtom("someAtom"))
        assertM(runSynth(expr))(equalTo(TValue(VTAtom("someAtom"))))
      },
      testM("works in a tuple") {
        val expr = ETuple(List(ELiteral(LAtom("someAtom")), ELiteral(LInt(1))))
        assertM(runSynth(expr))(
          equalTo(TTuple(List(TValue(VTAtom("someAtom")), TValue(VTInt(1)))))
        )
      }
    ),
    suite("NilTest")(
      testM("correctly infers type") {
        val expr = ELiteral(LNil)
        assertM(runSynth(expr))(equalTo(TValue(VTNil)))
      },
      testM("works in a tuple") {
        val expr = ETuple(List(ELiteral(LNil), ELiteral(LInt(1))))
        assertM(runSynth(expr))(
          equalTo(TTuple(List(TValue(VTNil), TValue(VTInt(1)))))
        )
      }
    )
  )
}
