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

object TypeAnnotationTest extends DefaultRunnableSpec {
  def spec = suite("TypeAnnotationTest")(
    testM("annotated list") {
      assertM(parseAndSynth("""(: [(| 1 2 3)] [1 2 3 1 2 3])"""))(
        equalTo(
          (
            EAnnotation(
              EList(
                List(
                  ELiteral(LInt(1)),
                  ELiteral(LInt(2)),
                  ELiteral(LInt(3)),
                  ELiteral(LInt(1)),
                  ELiteral(LInt(2)),
                  ELiteral(LInt(3))
                )
              ),
              TList(
                TSum(TValue(VTInt(3)), TValue(VTInt(1)), TValue(VTInt(2)))
              )
            ),
            TList(
              TSum(TValue(VTInt(3)), TValue(VTInt(1)), TValue(VTInt(2)))
            )
          )
        )
      )
    },
    testM("annotated function") {
      val tSum1and2 = TSum(TValue(VTInt(1)), TValue(VTInt(2)))
      assertM(parseAndSynth("""(: (fn [(| 1 2)] {(| 1 2) (| 1 2)}) 
                               |  (fn [a] {a a}))""".stripMargin))(
        equalTo(
          (
            EAnnotation(
              EFunction(
                List("a"),
                ETuple(List(EVariable("a"), EVariable("a")))
              ),
              TFunction(List(tSum1and2), TTuple(List(tSum1and2, tSum1and2)))
            ),
            TFunction(
              List(tSum1and2),
              TTuple(List(tSum1and2, tSum1and2))
            )
          )
        )
      )
    }
  )
}
