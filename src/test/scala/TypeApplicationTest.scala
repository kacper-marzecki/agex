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
import java.util.function.ToIntFunction

object TypeApplicationTest extends DefaultRunnableSpec {

  val EitherType = TMulQuantification(
    List("A", "B"),
    TSum(
      Set(
        TTuple(List(TValue(VTAtom("left")), TVariable("A"))),
        TTuple(List(TValue(VTAtom("right")), TVariable("B")))
      )
    )
  )
  val LeftConstructorType = TMulQuantification(
    List("A"),
    TFunction(
      List(TVariable("A")),
      TTuple(List(TValue(VTAtom("left")), TVariable("A")))
    )
  )
  val LeftConstructor =
    // EFunction(List("a"), ETuple(List(ELiteral(LAtom("left")), EVariable("a"))))
    EAnnotation(
      EFunction(
        List("a"),
        ETuple(List(ELiteral(LAtom("left")), EVariable("a")))
      ),
      LeftConstructorType
    )

  val RightConstructor = EAnnotation(
    EFunction(
      List("a"),
      ETuple(List(ELiteral(LAtom("right")), EVariable("a")))
    ),
    TMulQuantification(
      List("A"),
      TFunction(
        List(TVariable("A")),
        TTuple(List(TValue(VTAtom("right")), TVariable("A")))
      )
    )
  )

  /** type Either[A, B] =
    * | {:left, A}
    * | {:right, B}
    *
    * function left = a => {:left, a}
    *
    * let result : Either[Int, String] = left(1)
    */
  def spec = suite("EitherTest")(
    testM("constructing a Left value works") {
      val expr = EAnnotation(
        EFunctionApplication(LeftConstructor, List(ELiteral(LInt(2)))),
        TTypeApp(EitherType, List(TLiteral(LTInt), TLiteral(LTString)))
      )
      assertM(runSynth(expr))(
        equalTo(
          TTypeApp(
            TMulQuantification(
              List("A", "B"),
              TSum(
                Set(
                  TTuple(List(TValue(VTAtom("left")), TVariable("A"))),
                  TTuple(List(TValue(VTAtom("right")), TVariable("B")))
                )
              )
            ),
            List(TLiteral(LTInt), TLiteral(LTString))
          )
        )
      )
    },
    testM("constructing a nested applied type works") {
      val ctx = Context(
        Vector(
          ContextElement.CTypeDefinition("Either", EitherType),
          ContextElement.CTypeDefinition("Left", LeftConstructorType)
        )
      )
      // a : [B, A](A) => Either[A, B] = something => Left(something)
      // a(2) : Either[Int, String]
      val expr = EAnnotation(
        EFunctionApplication(
          EAnnotation(
            EFunction(
              List("something"),
              EFunctionApplication(
                EVariable("Left"),
                List(EVariable("something"))
              )
            ),
            TMulQuantification(
              List("B", "A"),
              TFunction(
                List(TVariable("A")),
                TTypeApp(
                  TTypeRef("Either"),
                  List(TVariable("A"), TVariable("B"))
                )
              )
            )
          ),
          List(ELiteral(LInt(2)))
        ),
        TTypeApp(EitherType, List(TLiteral(LTInt), TLiteral(LTString)))
      )
      assertM(runSynthDebug(expr, ctx))(
        equalTo(
          TTypeApp(
            TMulQuantification(
              List("A", "B"),
              TSum(
                Set(
                  TTuple(List(TValue(VTAtom("left")), TVariable("A"))),
                  TTuple(List(TValue(VTAtom("right")), TVariable("B")))
                )
              )
            ),
            List(TLiteral(LTInt), TLiteral(LTString))
          )
        )
      )
    }
  )
}
