import zio._
import zio.console._
import zio.test._
import zio.test.Assertion._
import zio.test.environment._
import Expression.*
import Type.*
import LiteralType.*
import Literal.*
import javax.swing.plaf.metal.MetalIconFactory.TreeLeafIcon
import javax.swing.plaf.metal.MetalBorders.ToolBarBorder
import java.security.spec.EdDSAParameterSpec

object SynthTest extends DefaultRunnableSpec {
  val litString = ELiteral(LString("string"))
  val litBool = ELiteral(LBool(false))
  val idFunction = EAbstraction("x", EVariable("x"))
  val strBoolTuple = ETuple(litString, litBool)

  def runSynth(expr: Expression) =
    synth(expr)
      .provideSomeLayer[ZEnv](MutableState.live)
      .tapError(it => putStrLn(it.show))

  def spec = suite("SynthTest")(
    testM("simple") {
      assertM(runSynth(litString))(
        equalTo(TLiteral(LTString))
      )
    },
    testM("application String") {
      val expr = EApplication(idFunction, litString)
      assertM(runSynth(expr))(
        equalTo(TLiteral(LTString))
      )
    },
    testM("application Bool") {
      val expr = EApplication(idFunction, litBool)
      assertM(runSynth(expr))(
        equalTo(TLiteral(LTBool))
      )
    },
    testM("lambda") {
      assertM(runSynth(idFunction))(
        equalTo(TFunction(TExistential("t1"), TExistential("t1")))
      )
    },
    testM("tuples") {
      assertM(runSynth(strBoolTuple))(
        equalTo(TProduct(TLiteral(LTString), TLiteral(LTBool)))
      )
    },
    testM("tuple in lambda") {
      val expr = EApplication(
        EAbstraction("x", ETuple(EVariable("x"), EVariable("x"))),
        litBool
      )
      assertM(runSynth(expr))(
        equalTo(TProduct(TLiteral(LTBool), TLiteral(LTBool)))
      )
    },
    testM("nested Tuple") {
      val expr = EApplication(
        EAbstraction(
          "x",
          ETuple(EVariable("x"), ETuple(EVariable("x"), EVariable("x")))
        ),
        litBool
      )
      assertM(runSynth(expr))(
        equalTo(
          TProduct(
            TLiteral(LTBool),
            TProduct(TLiteral(LTBool), TLiteral(LTBool))
          )
        )
      )
    },
    testM("general let") {
      val expr = ELet(
        "idFunction",
        idFunction,
        EApplication(EVariable("idFunction"), strBoolTuple)
      )
      assertM(runSynth(expr))(
        equalTo(
          TProduct(TLiteral(LTString), TLiteral(LTBool))
        )
      )
    },
    testM("id with universal quantifier") {
      val expr = ELet(
        "someFunction",
        EAnnotation(
          idFunction,
          TQuantification("a", TFunction(TVariable("a"), TVariable("a")))
        ),
        EApplication(EVariable("someFunction"), litBool)
      )
      assertM(runSynth(expr))(
        equalTo(TLiteral(LTBool))
      )
    },
    testM("curried function") {
      val plusFunction = EAnnotation(
        EAbstraction("a", EAbstraction("b", ELiteral(LInt(1)))),
        TFunction(TLiteral(LTInt), TFunction(TLiteral(LTInt), TLiteral(LTInt)))
      )
      val expr = EApplication(
        EApplication(plusFunction, ELiteral(LInt(1))),
        ELiteral(LInt(1))
      )
      assertM(runSynth(expr))(
        equalTo(TLiteral(LTInt))
      )
    }
  )
}
