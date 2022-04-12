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
import CommonTestFunctions._
import scala.language.experimental
import cats.data.NonEmptyListInstances

object IfTest extends DefaultRunnableSpec {
  def spec = suite("IfTest")(
    testM("basic if") {
      val exp =
        EIf(ELiteral(LBool(true)), (ELiteral(LInt(2))), (ELiteral(LInt(4))))
      assertM(runSynth(exp))(
        equalTo(
          TSum(
            TValue(VTInt(2)),
            TValue(VTInt(4))
          )
        )
      )
    },
    testM("doesnt allow other types of conditions") {
      val exp = EIf(ELiteral(LInt(1)), ELiteral(LInt(2)), ELiteral(LInt(4)))
      assertM(runSynth(exp).flip)(
        equalTo(
          AppError.CompilationError(
            exp,
            AppError.TypeNotApplicableToLiteral(
              TLiteral(LTBool),
              LInt(1)
            )
          )
        )
      )
    },
    testM("permits using a subtype of ifTrue in ifFalse") {
      val exp = EIf(
        ELiteral(LBool(false)),
        EStruct(
          Map(
            "A" -> ELiteral(LBool(true))
          )
        ),
        EStruct(
          Map(
            "A" -> ELiteral(LBool(true)),
            "B" -> ELiteral(LBool(true))
          )
        )
      )
      assertM(runSynth(exp))(
        equalTo(
          TStruct(
            Map(
              "A" -> TValue(VTBool(true))
            )
          )
        )
      )
    },
    testM("its possible to annotate ifFalse and IfTrue to a common supertype") {
      val structType = TStruct(
        Map(
          "A" -> TLiteral(LTBool)
        )
      )
      val exp = EIf(
        ELiteral(LBool(false)),
        EAnnotation(
          EStruct(
            Map(
              "A" -> ELiteral(LBool(true)),
              "B" -> ELiteral(LBool(true))
            )
          ),
          structType
        ),
        EAnnotation(
          EStruct(
            Map(
              "A" -> ELiteral(LBool(true)),
              "C" -> ELiteral(LBool(true))
            )
          ),
          structType
        )
      )
      assertM(runSynth(exp))(
        equalTo(
          TStruct(
            Map(
              "A" -> TLiteral(LTBool)
            )
          )
        )
      )
    }
  )
}
