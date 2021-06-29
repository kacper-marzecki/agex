import zio.*
import zio.console.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.environment.*
import Expression.*
import Type.*
import LiteralType.*
import ValueType.*
import Literal.*
import ContextElement.*
import TestCommonExpressions.*
import CommonTestFunctions.*
import javax.lang.model.`type`.TypeMirror
import cats.data.NonEmptyListInstances

object SumTest extends DefaultRunnableSpec {

  val stringOrInt = TSum(
    Set(
      TLiteral(LTString),
      TLiteral(LTInt)
    )
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
    }
  )
}
