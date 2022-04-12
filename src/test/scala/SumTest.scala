import zio._
import zio.console._
import zio.test._
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
import javax.lang.model.`type`.TypeMirror
import cats.data.NonEmptyListInstances

object SumTest extends DefaultRunnableSpec {

  val stringOrInt = TSum(
    TLiteral(LTString),
    TLiteral(LTInt)
  )

  def spec = suite("SumTest")(
    testM("an element of a sum type is a subtype of the sum type") {
      val exp = EAnnotation(
        ELiteral(LInt(1)),
        stringOrInt
      )
      assertM(runSynth(exp))(
        equalTo(stringOrInt)
      )
    },
    testM("sum type is flattened") {
      val exp =
        EAnnotation(
          ELiteral(LInt(1)),
          TSum(
            TLiteral(LTString),
            TSum(
              TLiteral(LTInt),
              TLiteral(LTAtom)
            )
          )
        )

      assertM(runSynth(exp))(
        equalTo(
          TSum(
            TLiteral(LTString),
            TLiteral(LTInt),
            TLiteral(LTAtom)
          )
        )
      )
    }
  )
}
