import zio._
import zio.console._
import zio.test._
import zio.test.assertM
import zio.test.Assertion._
import zio.test.environment._
import Expression._
import Type._
import ValueType._
import LiteralType._
import Literal._
import ContextElement._
import TestCommonExpressions._
import CommonTestFunctions._
import Eff._
import Typer._
import Utils.{pPrint, prettyPrint}
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
              "A"  -> TValue(VTString("A value")),
              "B"  -> TValue(VTBool(true)),
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

      assertM(runSynth(expr).tapError(pPrint(_, "TARGET")))(
        equalTo(
          TStruct(
            Map[String, Type](
              "A" -> TValue(VTString("asd")),
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

      assertM(runSynth(expr).flip.map(getCompilationErrorCause))(
        equalTo(
          AppError.MissingFields(List("B", "C"))
        )
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

      assertM(runSynth(expr).flip.map(getCompilationErrorCause))(
        equalTo(
          AppError.TypesNotEqual(TLiteral(LTString), TLiteral(LTBool))
        )
      )
    }
  )
}
