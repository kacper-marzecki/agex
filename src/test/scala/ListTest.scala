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
import CommonTestFunctions.{runSynth, parseAndSynth}

object ListTest extends DefaultRunnableSpec {
  def spec = suite("ListTest")(
    testM("empty list") {
      assertM(parseAndSynth("""[ ]"""))(
        equalTo((EList(List()), TList(TNothing)))
      )
    },
    testM("returned from function") {
      assertM(parseAndSynth("""(fn [a] [a 1])"""))(
        equalTo(
          (
            EFunction(
              List("a"),
              EList(List(EVariable("a"), ELiteral(LInt(1))))
            ),
            TFunction(
              List(TExistential("t1")),
              TList(TSum(TExistential("t1"), TValue(VTInt(1))))
            )
          )
        )
      )
    },
    testM("heterogenous list") {
      assertM(parseAndSynth("""[1 2 3 :atom]"""))(
        equalTo(
          (
            EList(
              List(
                ELiteral(LInt(1)),
                ELiteral(LInt(2)),
                ELiteral(LInt(3)),
                ELiteral(LAtom("atom"))
              )
            ),
            TList(
              TSum(
                TValue(VTInt(1)),
                TValue(VTInt(2)),
                TValue(VTInt(3)),
                TValue(VTAtom("atom"))
              )
            )
          )
        )
      )
    }
  )
}
