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
import java.util.function.LongBinaryOperator
import javax.swing.plaf.metal.MetalIconFactory.TreeLeafIcon
import java.security.spec.EdDSAParameterSpec

object StructTest extends DefaultRunnableSpec {

  def spec = suite("StructTest")(
    testM("correctly infers struct type") {
      val expr = EStruct(
        Map(
          "A"  -> ELiteral(LString("A value")),
          "B"  -> ELiteral(LBool(true)),
          "id" -> TestCommonExpressions.annotatedId
        )
      )
      assertM(runSynth(expr))(
        equalTo(
          TStruct(
            Map[String, Type](
              "A"  -> TLiteral(LTString),
              "B"  -> TLiteral(LTBool),
              "id" -> TestCommonExpressions.annotatedId.annotatedType
            )
          )
        )
      )
    },
    testM("generic struct returned from function") {
      val expr = EFunctionApplication(
        EAnnotation(
          EFunction(
            List("a"),
            EStruct(Map("A" -> EVariable("a"), "B" -> ELiteral(LBool(false))))
          ),
          TQuantification(
            "a",
            TFunction(
              List(TVariable("a")),
              TStruct(
                Map[String, Type](
                  "A" -> TVariable("a"),
                  "B" -> TLiteral(LTBool)
                )
              )
            )
          )
        ),
        List(ELiteral(LString("asd")))
      )

      assertM(runSynth(expr))(
        equalTo(
          TStruct(
            Map[String, Type](
              "A" -> TLiteral(LTString),
              "B" -> TLiteral(LTBool)
            )
          )
        )
      )
    },
    testM("correctly determines struct subtyping") {
      val struct = EStruct(
        Map(
          "A" -> ELiteral(LString("A value")),
          "B" -> ELiteral(LBool(true))
        )
      )
      val expr = ELet(
        "lambda",
        EAnnotation(
          EFunction(List("a"), ELiteral(LBool(false))),
          TFunction(
            List(
              TStruct(
                Map[String, Type](
                  "A" -> TLiteral(LTString)
                )
              )
            ),
            TLiteral(LTBool)
          )
        ),
        EFunctionApplication(EVariable("lambda"), List(struct))
      )

      assertM(runSynth(expr))(
        equalTo(TLiteral(LTBool))
      )
    },
    testM("fails when struct has not all fields") {
      val struct = EStruct(
        Map(
          "A" -> ELiteral(LString("A value"))
        )
      )
      val expr = ELet(
        "lambda",
        EAnnotation(
          EFunction(List("a"), ELiteral(LBool(false))),
          TFunction(
            List(
              TStruct(
                Map[String, Type](
                  "A" -> TLiteral(LTString),
                  "B" -> TLiteral(LTBool),
                  "C" -> TLiteral(LTFloat)
                )
              )
            ),
            TLiteral(LTBool)
          )
        ),
        EFunctionApplication(EVariable("lambda"), List(struct))
      )

      assertM(runSynth(expr).flip)(
        equalTo(AppError.MissingFields(List("B", "C")))
      )
    },
    testM("fails when struct field has incorrect Type ") {
      val struct = EStruct(
        Map(
          "A" -> ELiteral(LString("A value"))
        )
      )
      val expr = ELet(
        "lambda",
        EAnnotation(
          EFunction(List("a"), ELiteral(LBool(false))),
          TFunction(
            List(
              TStruct(
                Map[String, Type](
                  "A" -> TLiteral(LTBool)
                )
              )
            ),
            TLiteral(LTBool)
          )
        ),
        EFunctionApplication(EVariable("lambda"), List(struct))
      )

      assertM(runSynth(expr).flip)(
        equalTo(AppError.TypesNotEqual(TLiteral(LTString), TLiteral(LTBool)))
      )
    }
  )
}
