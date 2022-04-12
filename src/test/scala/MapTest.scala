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
import TMapping.Required
import CommonTestFunctions.{runSynth, parseAndSynth}

object MapTest extends DefaultRunnableSpec {
  def spec =
    suite("MapTest")(
      testM("Empty map") {
        assertM(parseAndSynth("%{}"))(
          equalTo((EMap(List()), TMap(List())))
        )
      },
      testM("int->string map") {
        assertM(parseAndSynth(""" %{1 "1" 2 "2" 3 3.2} """))(
          equalTo(
            (
              EMap(
                List(
                  (ELiteral(LInt(1)), ELiteral(LString("1"))),
                  (ELiteral(LInt(2)), ELiteral(LString("2"))),
                  (ELiteral(LInt(3)), ELiteral(LFloat(3.2f)))
                )
              ),
              TMap(
                List(
                  Required(TValue(VTInt(1)), TValue(VTString("1"))),
                  Required(TValue(VTInt(2)), TValue(VTString("2"))),
                  Required(TValue(VTInt(3)), TValue(VTFloat(3.2f)))
                )
              )
            )
          )
        )
      },
      testM("map with function applications") {
        assertM(
          parseAndSynth(
            """ %{ ((fn [a] a) 1) "1" 
                 | 2              ((fn [a] a) "2")}""".stripMargin
          )
        )(
          equalTo(
            (
              EMap(
                List(
                  (
                    EFunctionApplication(
                      EFunction(List("a"), EVariable("a")),
                      List(ELiteral(LInt(1)))
                    ),
                    ELiteral(LString("1"))
                  ),
                  (
                    ELiteral(LInt(2)),
                    EFunctionApplication(
                      EFunction(List("a"), EVariable("a")),
                      List(ELiteral(LString("2")))
                    )
                  )
                )
              ),
              TMap(
                List(
                  Required(TValue(VTInt(1)), TValue(VTString("1"))),
                  Required(TValue(VTInt(2)), TValue(VTString("2")))
                )
              )
            )
          )
        )
      }
    )
}
