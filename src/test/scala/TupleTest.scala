import zio.*
import zio.console.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.environment.*
import Expression.*
import Type.*
import LiteralType.*
import Literal.*
import ContextElement.*
import TestCommonExpressions.*

object TupleTest extends DefaultRunnableSpec {

  def repeat[A](xs: List[A], times: Int) =
    List.fill(times)(xs).flatten

  def runSynth(expr: Expression, context: Context = Context()) =
    synth(expr, context)
      // .tap(prettyPrint(_, "synthResult"))
      .map(_._type)
      .provideSomeLayer[ZEnv](CompilerState.live)
  // .tapError(prettyPrint(_, "synthError"))

  def spec = suite("TupleTest")(
    testM("works for an emptyTuple type") {
      val expr = ENewTuple(Nil)
      assertM(runSynth(expr))(
        equalTo(TNewProduct(Nil))
      )
    },
    testM("works for a long tuple") {
      val expr =
        ENewTuple(repeat(List(litInt, litBool, litString), 100))
      assertM(runSynth(expr))(
        equalTo(
          TNewProduct(
            repeat(
              List(TLiteral(LTInt), TLiteral(LTBool), TLiteral(LTString)),
              100
            )
          )
        )
      )
    }
  )
}
