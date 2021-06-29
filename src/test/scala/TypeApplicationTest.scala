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
import cats.data.EitherT

object TypeApplicationTest extends DefaultRunnableSpec {

  val EitherType = TMulQuantification(
    Set("A", "B"),
    TSum(
      Set(
        TTuple(List(TValue(VTAtom("left")), TVariable("A"))),
        TTuple(List(TValue(VTAtom("right")), TVariable("B")))
      )
    )
  )
  val LeftConstructorType = TMulQuantification(
    Set("X"),
    TFunction(
      List(TVariable("X")),
      TTuple(List(TValue(VTAtom("left")), TVariable("X")))
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
      Set("B"),
      TFunction(
        List(TVariable("B")),
        TTuple(List(TValue(VTAtom("right")), TVariable("B")))
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
      assertM(runSynthDebug(expr))(
        equalTo(
          TTypeApp(
            TMulQuantification(
              Set("A", "B"),
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
      val expr = EAnnotation(
        EFunction(
          List("something"),
          EFunctionApplication(EVariable("Left"), List(EVariable("something")))
        ),
        TMulQuantification(
          Set("A", "B"),
          TFunction(
            List(TVariable("A")),
            TTypeApp(TTypeRef("Either"), List(TVariable("A"), TVariable("B")))
          )
        )
      )
      assertM(runSynth(expr, ctx))(
        equalTo(
          TMulQuantification(
            Set("A", "B"),
            TFunction(
              List(TVariable("A")),
              TTypeApp(
                TMulQuantification(
                  Set("A", "B"),
                  TSum(
                    Set(
                      TTuple(List(TValue(VTAtom("left")), TVariable("A"))),
                      TTuple(List(TValue(VTAtom("right")), TVariable("B")))
                    )
                  )
                ),
                List(TVariable("A"), TVariable("B"))
              )
            )
          )
        )
      )
    }
  )
}
