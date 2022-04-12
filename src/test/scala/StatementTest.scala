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
import CommonTestFunctions.{runSynth, toZIO}
import Statement._

object StatementTest extends DefaultRunnableSpec {
  val a = testM("parsesModule") {
    val module = """
      (defmodule Kek
        (alias Application.Math)
        (def amount 1)
        (defn increment
          ([integer] integer)
          [a] (Math.plus a amount))
      )
      """
    val x      = Compiler.fileToModule(module)
    val ass: AssertionM[List[AgexModule]] = equalTo(
      List(
        ModuleDefinition(
          "Kek",
          List("Application.Math"),
          List(
            ModuleAttribute("amount", ELiteral(LInt(1))),
            FunctionDef(
              "increment",
              TFunction(List(TLiteral(LTInt)), TLiteral(LTInt)),
              List("a"),
              EFunctionApplication(
                EVariable("Math.plus"),
                List(EVariable("a"), EVariable("amount"))
              )
            )
          )
        )
      )
    )
    // assertM(ZIO.succeed(1))(equalTo(1))
    assertM(toZIO(x))(ass)
  }
  def spec = suite("StatementTest")(a)
}
