import zio.*
import zio.console.*
import zio.test.*
import zio.test.assertM
import zio.test.Assertion.*
import zio.test.environment.*
import Expression.*
import Type.*
import LiteralType.*
import ValueType.*
import Literal.*
import ContextElement.*
import TestCommonExpressions.*
import CommonTestFunctions.{runSynth, toZIO}
import Statement.*

object StatementTest extends DefaultRunnableSpec {
  val a = testM("parsesModule") {
    val module = """
      (defmodule Kek
        (alias Application.Math)
        (def amount 1)
        (defn increment
          ([Integer] Integer)
          [a] (Math.plus a amount))
      )
      """
    val x      = Compiler.fileToModule(module)
    val ass = equalTo(
      List(
        Left(
          ModuleDefinition(
            "Kek",
            List("Application.Math"),
            List(
              ModuleAttribute("amount", ELiteral(LInt(1))),
              FunctionDef(
                "increment",
                TFunction(List(TVariable("Integer")), TVariable("Integer")),
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
    )
    // assertM(ZIO.succeed(1))(equalTo(1))
    assertM(toZIO(x))(ass)
  }
  def spec = suite("StatementTest")(a)
}
