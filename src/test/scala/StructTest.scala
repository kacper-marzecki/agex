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
      val expr = EApplication(
        EAnnotation(
          ELambda(
            "a",
            EStruct(Map("A" -> EVariable("a"), "B" -> ELiteral(LBool(false))))
          ),
          TQuantification(
            "a",
            TFunction(
              TVariable("a"),
              TStruct(
                Map[String, Type](
                  "A" -> TVariable("a"),
                  "B" -> TLiteral(LTBool)
                )
              )
            )
          )
        ),
        ELiteral(LString("asd"))
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
          ELambda("a", ELiteral(LBool(false))),
          TFunction(
            TStruct(
              Map[String, Type](
                "A" -> TLiteral(LTString)
              )
            ),
            TLiteral(LTBool)
          )
        ),
        EApplication(EVariable("lambda"), struct)
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
          ELambda("a", ELiteral(LBool(false))),
          TFunction(
            TStruct(
              Map[String, Type](
                "A" -> TLiteral(LTString),
                "B" -> TLiteral(LTBool),
                "C" -> TLiteral(LTFloat)
              )
            ),
            TLiteral(LTBool)
          )
        ),
        EApplication(EVariable("lambda"), struct)
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
          ELambda("a", ELiteral(LBool(false))),
          TFunction(
            TStruct(
              Map[String, Type](
                "A" -> TLiteral(LTBool)
              )
            ),
            TLiteral(LTBool)
          )
        ),
        EApplication(EVariable("lambda"), struct)
      )

      assertM(runSynth(expr).flip)(
        equalTo(AppError.TypesNotEqual(TLiteral(LTString), TLiteral(LTBool)))
      )
    }
  )
}
