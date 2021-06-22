import zio.*
import zio.console.*
import zio.test.*
import zio.test.assertM
import zio.test.Assertion.*
import zio.test.environment.*
import Expression.*
import Type.*
import LiteralType.*
import Literal.*
import ContextElement.*
import TestCommonExpressions.*
import CommonTestFunctions.runSynth

object AtomTest extends DefaultRunnableSpec {

  def spec = suite("AtomTest")(
    testM("correctly infers type") {
      val expr = ELiteral(LAtom("someAtom"))
      assertM(runSynth(expr))(equalTo(TLiteral(LTAtom)))
    },
    testM("works in a tuple") {
      val expr = ETuple(List(ELiteral(LAtom("someAtom")), ELiteral(LInt(1))))
      assertM(runSynth(expr))(
        equalTo(TTuple(List(TLiteral(LTAtom), TLiteral(LTInt))))
      )
    }
  )
}
