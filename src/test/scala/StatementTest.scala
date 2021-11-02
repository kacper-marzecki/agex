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
import CommonTestFunctions.{runSynth, parseAndSynth}
import Statement.*
object StatementTest extends DefaultRunnableSpec {
  def spec = suite("StatementTest")(
    testM("parsesModule") {
      val module = """
      (defmodule Kek 
        (alias Application.Math)
        (def amount 1)
        (defn increment 
          ([Integer] Integer) 
          [a] (Math.plus a amount))
      )
      """
      assertM(
        Tokenizer.parseToSexprs(module).flatMap(Transformer.toStatement)
      )(
        equalTo(
          ModuleDefinition(
            "Kek",
            List(
              Alias("Application.Math"),
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
    }
  )
}
