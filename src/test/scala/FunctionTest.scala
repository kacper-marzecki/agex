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

object FunctionTest extends DefaultRunnableSpec {
  val stdCtx = Context(
    Vector(
      CTypedVariable(
        "+",
        TFunction(
          List(TLiteral(LTInt), TLiteral(LTInt)),
          TLiteral(LTInt)
        )
      )
    )
  )
  val aPlusBPlusC = EFunction(
    List("a", "b", "c"),
    EFunctionApplication(
      EVariable("+"),
      List(
        EFunctionApplication(
          EVariable("+"),
          List(EVariable("a"), EVariable("b"))
        ),
        EVariable("c")
      )
    )
  )
  val genericTwoTupleConstructor = EAnnotation(
    EFunction(
      List("a", "b"),
      ETuple(List(EVariable("a"), EVariable("b")))
    ),
    TMulQuantification(
      Set("A", "B"),
      TFunction(
        List(TVariable("A"), TVariable("B")),
        TTuple(List(TVariable("A"), TVariable("B")))
      )
    )
  )

  def spec = suite("FunctionTest")(
    testM("infers type of a multiarg function") {
      assertM(runSynth(aPlusBPlusC, stdCtx))(
        equalTo(
          TFunction(
            List(TLiteral(LTInt), TLiteral(LTInt), TLiteral(LTInt)),
            TLiteral(LTInt)
          )
        )
      )
    },
    testM("infers result type of an application of a  multiarg function") {
      val expr = EFunctionApplication(
        aPlusBPlusC,
        List(ELiteral(LInt(1)), ELiteral(LInt(1)), ELiteral(LInt(1)))
      )
      assertM(runSynth(expr, stdCtx))(
        equalTo(TLiteral(LTInt))
      )
    },
    testM("infers existential types of a function") {
      val expr =
        EFunction(List("a", "b"), ETuple(List(EVariable("a"), EVariable("b"))))
      assertM(runSynth(expr))(
        equalTo(
          TFunction(
            List(TExistential("t1"), TExistential("t2")),
            TTuple(List(TExistential("t1"), TExistential("t2")))
          )
        )
      )
    },
    testM("infers generic types of a function") {
      val expr = genericTwoTupleConstructor
      assertM(runSynth(expr))(
        equalTo(
          TMulQuantification(
            Set("A", "B"),
            TFunction(
              List(TVariable("A"), TVariable("B")),
              TTuple(List(TVariable("A"), TVariable("B")))
            )
          )
        )
      )
    },
    testM("correctly applies a generic function") {
      val expr = EFunctionApplication(
        genericTwoTupleConstructor,
        List(
          ELiteral(LInt(1)),
          EStruct(
            Map(
              "A" -> ELiteral(LString("A value")),
              "B" -> ELiteral(LBool(true))
            )
          )
        )
      )
      assertM(runSynth(expr))(
        equalTo(
          TTuple(
            List(
              TLiteral(LTInt),
              TStruct(
                Map(
                  "A" -> TLiteral(LTString),
                  "B" -> TLiteral(LTBool)
                )
              )
            )
          )
        )
      )
    },
    testM("detects arg type mismatch") {
      val expr = EFunctionApplication(
        aPlusBPlusC,
        List(ELiteral(LBool(false)), ELiteral(LInt(1)), ELiteral(LInt(1)))
      )
      assertM(runSynth(expr, stdCtx).flip)(
        equalTo(AppError.TypeNotApplicableToLiteral(LTInt, LBool(false)))
      )
    },
    testM("detects arity mismatch") {
      val expr = EFunctionApplication(
        aPlusBPlusC,
        List(ELiteral(LInt(1)), ELiteral(LInt(1)))
      )
      assertM(runSynth(expr, stdCtx).flip)(equalTo(AppError.WrongArity(3, 2)))
    },
    testM("nullary functions work") {
      val expr = EFunctionApplication(
        EFunction(Nil, ELiteral(LInt(1))),
        Nil
      )
      assertM(runSynth(expr))(equalTo(TLiteral(LTInt)))
    }
  )
}
