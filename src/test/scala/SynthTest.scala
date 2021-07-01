import zio.*
import zio.console.*
import zio.test.*
import zio.test.assertM
import zio.test.Assertion.*
import zio.test.environment.*
import Expression.*
import Type.*
import LiteralType.*
import ValueType.*
import Literal.*
import ContextElement.*
import TestCommonExpressions.*
import CommonTestFunctions.runSynth
import scala.language.experimental
import cats.data.NonEmptyListInstances

object SynthTest extends DefaultRunnableSpec {

  def spec = suite("SynthTest")(
    testM("literal has its type") {
      assertM(runSynth(litString))(
        equalTo(TValue(VTString("string")))
      )
    },
    testM("Id application with string gives back string") {
      val expr = EFunctionApplication(idFunction, List(litString))
      assertM(runSynth(expr))(
        equalTo(TValue(VTString("string")))
      )
    },
    testM("Id application with boolean gives back boolean") {
      val expr = EFunctionApplication(idFunction, List(litBool))
      assertM(runSynth(expr))(
        equalTo(TValue(VTBool(false)))
      )
    },
    testM("Id has a x -> x type") {
      assertM(runSynth(idFunction))(
        equalTo(TFunction(List(TExistential("t1")), TExistential("t1")))
      )
    },
    testM("Tuple type is correctly inferred") {
      assertM(runSynth(strBoolTuple))(
        equalTo(TTuple(List(TValue(VTString("string")), TValue(VTBool(false)))))
      )
    },
    testM("tuple is created by application of a lambda") {
      val expr = EFunctionApplication(
        EFunction(List("x"), ETuple(List(EVariable("x"), EVariable("x")))),
        List(litBool)
      )
      assertM(runSynth(expr))(
        equalTo(TTuple(List(TValue(VTBool(false)), TValue(VTBool(false)))))
      )
    },
    testM("nested tuple is created by application of a lambda") {
      val expr = EFunctionApplication(
        EFunction(
          List("x"),
          ETuple(
            List(
              EVariable("x"),
              ETuple(List(EVariable("x"), EVariable("x")))
            )
          )
        ),
        List(litBool)
      )
      assertM(runSynth(expr))(
        equalTo(
          TTuple(
            List(
              TValue(VTBool(false)),
              TTuple(List(TValue(VTBool(false)), TValue(VTBool(false))))
            )
          )
        )
      )
    },
    testM("Id function is bound and applied by reference") {
      val expr = ELet(
        "idFunction",
        idFunction,
        EFunctionApplication(EVariable("idFunction"), List(strBoolTuple))
      )
      assertM(runSynth(expr))(
        equalTo(
          TTuple(List(TValue(VTString("string")), TValue(VTBool(false))))
        )
      )
    },
    testM("Id annotated explicitly as forall a. a -> a works") {
      val expr = ELet(
        "someFunction",
        annotatedId,
        EFunctionApplication(EVariable("someFunction"), List(litBool))
      )
      assertM(runSynth(expr))(
        equalTo(TValue(VTBool(false)))
      )
    },
    testM("function can be curried") {
      val plusFunction = EAnnotation(
        EFunction(List("a"), EFunction(List("b"), ELiteral(LInt(1)))),
        TFunction(
          List(TLiteral(LTInt)),
          TFunction(List(TLiteral(LTInt)), TLiteral(LTInt))
        )
      )
      val expr = EFunctionApplication(
        EFunctionApplication(plusFunction, List(ELiteral(LInt(1)))),
        List(ELiteral(LInt(1)))
      )
      assertM(runSynth(expr))(
        equalTo(TLiteral(LTInt))
      )
    },
    testM("partial application") {
      // the following code is roughly represented in this test:
      // let plus = fun(a) -> fun(b) -> a + b
      // let plus1 = plus(1)
      // let apply = fun(function, arg) -> function(arg)
      // apply(plus1, 1)
      val plusFunction = EAnnotation(
        EFunction(List("a"), EFunction(List("b"), ELiteral(LInt(1)))),
        TFunction(
          List(TLiteral(LTInt)),
          TFunction(List(TLiteral(LTInt)), TLiteral(LTInt))
        )
      )
      val expr = ELet(
        "plus",
        plusFunction,
        ELet(
          "plus1",
          EFunctionApplication(EVariable("plus"), List(ELiteral(LInt(1)))),
          ELet(
            "apply",
            EFunction(
              List("function"),
              EFunction(
                List("arg"),
                EFunctionApplication(
                  EVariable("function"),
                  List(EVariable("arg"))
                )
              )
            ),
            EFunctionApplication(
              EFunctionApplication(
                EVariable("apply"),
                List(EVariable("plus1"))
              ),
              List(ELiteral(LInt(1)))
            )
          )
        )
      )
      assertM(runSynth(expr))(
        equalTo(TLiteral(LTInt))
      )
    },
    testM("gets a function from initial context") {
      val ctx = Context(
        Vector(
          CTypedVariable(
            "+",
            TFunction(
              List(TLiteral(LTInt)),
              TFunction(List(TLiteral(LTInt)), TLiteral(LTInt))
            )
          )
        )
      )

      val exp = EFunctionApplication(
        EFunctionApplication(EVariable("+"), List(ELiteral(LInt(1)))),
        List(ELiteral(LInt(1)))
      )
      assertM(runSynth(exp, ctx))(
        equalTo(TLiteral(LTInt))
      )
    },
    // TODO: FIXME
    // testM("fail on shadowed variable names") {
    //   val ctx = Context(
    //     Vector(
    //       CTypedVariable(
    //         "+",
    //         TFunction(
    //           List(TLiteral(LTInt)),
    //           TFunction(List(TLiteral(LTInt)), TLiteral(LTInt))
    //         )
    //       )
    //     )
    //   )
    //   // let a = "asd"
    //   // let b = a -> 1 + a
    //   // b(1)
    //   val exp = ELet(
    //     "a",
    //     ELiteral(LString("asd")),
    //     ELet(
    //       "b",
    //       EFunction(
    //         List("a"),
    //         EFunctionApplication(
    //           EFunctionApplication(EVariable("+"), List(ELiteral(LInt(1)))),
    //           List(EVariable("a"))
    //         )
    //       ),
    //       EFunctionApplication(EVariable("b"), List(ELiteral(LInt(1))))
    //     )
    //   )
    //   val eff = runSynth(exp, ctx).flip
    //   assertM(eff)(Assertion.assertion("raises correct error")() {
    //     case it: AppError.ShadowedVariableName if it.name == "a" => true
    //     case _                                                   => false
    //   })
    // },
    // testM("args in other functions are not considered as shadowed") {
    //   val exp = ELet(
    //     "id",
    //     idFunction,
    //     ELet(
    //       "a",
    //       ELiteral(LInt(1)),
    //       ELiteral(LInt(1))
    //     )
    //   )
    //   assertM(runSynth(exp))(
    //     equalTo(TValue(VTInt(1)))
    //   )
    // },
    testM("type-checks with named annotation") {
      val ctx = Context(
        Vector(CTypeDefinition("Boolean", TLiteral(LTBool)))
      )
      val expr = EAnnotation(ELiteral(LBool(true)), TTypeRef("Boolean"))
      assertM(runSynth(expr, ctx))(
        equalTo(TLiteral(LTBool))
      )
    },
    testM("doesnt type-check with named annotation if type doesnt exist") {
      val expr = EAnnotation(ELiteral(LBool(true)), TTypeRef("annotatedType"))
      assertM(runSynth(expr).flip)(
        Assertion.assertion("raises correct error")() {
          case it: AppError.TypeNotKnown if it.name == "annotatedType" =>
            true
          case _ => false
        }
      )
    },
    testM("doesnt type-check with a wrong annotation") {
      val ctx = Context(
        Vector(CTypeDefinition("Boolean", TLiteral(LTBool)))
      )
      val expr = EAnnotation(ELiteral(LInt(1)), TTypeRef("Boolean"))
      assertM(runSynth(expr, ctx).flip)(
        equalTo(AppError.TypeNotApplicableToLiteral(TLiteral(LTBool), LInt(1)))
      )
    },
    testM("uses type alias in type annotations") {
      // the following code is roughly represented in this test:
      // type asd = Integer
      // type kek = asd -> Boolean
      // let fun: kek = a => false
      // fun(1)
      val ctx = Context(
        Vector(CTypeDefinition("Integer", TLiteral(LTInt)))
      )
      val expr = ETypeAlias(
        "asd",
        TTypeRef("Integer"),
        ETypeAlias(
          "kek",
          TFunction(List(TTypeRef("asd")), TLiteral(LTBool)),
          ELet(
            "fun",
            EAnnotation(
              EFunction(List("a"), ELiteral(LBool(false))),
              TTypeRef("kek")
            ),
            EFunctionApplication(EVariable("fun"), List(ELiteral(LInt(1))))
          )
        )
      )
      assertM(runSynth(expr, ctx))(
        equalTo(TLiteral(LTBool))
      )
    }
  )
}
