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
import scala.language.experimental
import cats.data.NonEmptyListInstances

object SynthTest extends DefaultRunnableSpec {

  def spec = suite("SynthTest")(
    testM("literal has its type") {
      assertM(runSynth(litString))(
        equalTo(TLiteral(LTString))
      )
    },
    testM("Id application with string gives back string") {
      val expr = EApplication(idFunction, litString)
      assertM(runSynth(expr))(
        equalTo(TLiteral(LTString))
      )
    },
    testM("Id application with boolean gives back boolean") {
      val expr = EApplication(idFunction, litBool)
      assertM(runSynth(expr))(
        equalTo(TLiteral(LTBool))
      )
    },
    testM("Id has a x -> x type") {
      assertM(runSynth(idFunction))(
        equalTo(TLambda(TExistential("t1"), TExistential("t1")))
      )
    },
    testM("Tuple type is correctly inferred") {
      assertM(runSynth(strBoolTuple))(
        equalTo(TTuple(List(TLiteral(LTString), TLiteral(LTBool))))
      )
    },
    testM("tuple is created by application of a lambda") {
      val expr = EApplication(
        ELambda("x", ETuple(List(EVariable("x"), EVariable("x")))),
        litBool
      )
      assertM(runSynth(expr))(
        equalTo(TTuple(List(TLiteral(LTBool), TLiteral(LTBool))))
      )
    },
    testM("nested tuple is created by application of a lambda") {
      val expr = EApplication(
        ELambda(
          "x",
          ETuple(
            List(
              EVariable("x"),
              ETuple(List(EVariable("x"), EVariable("x")))
            )
          )
        ),
        litBool
      )
      assertM(runSynth(expr))(
        equalTo(
          TTuple(
            List(
              TLiteral(LTBool),
              TTuple(List(TLiteral(LTBool), TLiteral(LTBool)))
            )
          )
        )
      )
    },
    testM("Id function is bound and applied by reference") {
      val expr = ELet(
        "idFunction",
        idFunction,
        EApplication(EVariable("idFunction"), strBoolTuple)
      )
      assertM(runSynth(expr))(
        equalTo(
          TTuple(List(TLiteral(LTString), TLiteral(LTBool)))
        )
      )
    },
    testM("Id annotated explicitly as forall a. a -> a works") {
      val expr = ELet(
        "someFunction",
        annotatedId,
        EApplication(EVariable("someFunction"), litBool)
      )
      assertM(runSynth(expr))(
        equalTo(TLiteral(LTBool))
      )
    },
    testM("function can be curried") {
      val plusFunction = EAnnotation(
        ELambda("a", ELambda("b", ELiteral(LInt(1)))),
        TLambda(TLiteral(LTInt), TLambda(TLiteral(LTInt), TLiteral(LTInt)))
      )
      val expr = EApplication(
        EApplication(plusFunction, ELiteral(LInt(1))),
        ELiteral(LInt(1))
      )
      assertM(runSynth(expr))(
        equalTo(TLiteral(LTInt))
      )
    },
    testM("partial application") {
      // the following code is roughly represented in this test:
      // let plus = fun(a, b) -> a + b
      // let plus1 = plus(1)
      // let apply = fun(function, arg) -> function(arg)
      // apply(plus1, 1)
      val plusFunction = EAnnotation(
        ELambda("a", ELambda("b", ELiteral(LInt(1)))),
        TLambda(TLiteral(LTInt), TLambda(TLiteral(LTInt), TLiteral(LTInt)))
      )
      val expr = ELet(
        "plus",
        plusFunction,
        ELet(
          "plus1",
          EApplication(EVariable("plus"), ELiteral(LInt(1))),
          ELet(
            "apply",
            ELambda(
              "function",
              ELambda(
                "arg",
                EApplication(EVariable("function"), EVariable("arg"))
              )
            ),
            EApplication(
              EApplication(EVariable("apply"), EVariable("plus1")),
              ELiteral(LInt(1))
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
            TLambda(
              TLiteral(LTInt),
              TLambda(TLiteral(LTInt), TLiteral(LTInt))
            )
          )
        )
      )

      val exp = EApplication(
        EApplication(EVariable("+"), ELiteral(LInt(1))),
        ELiteral(LInt(1))
      )
      assertM(runSynth(exp, ctx))(
        equalTo(TLiteral(LTInt))
      )
    },
    testM("fail on shadowed variable names ") {
      val ctx = Context(
        Vector(
          CTypedVariable(
            "+",
            TLambda(
              TLiteral(LTInt),
              TLambda(TLiteral(LTInt), TLiteral(LTInt))
            )
          )
        )
      )
      // let a = "asd"
      // let b = a -> 1 + a
      // b(1)
      val exp = ELet(
        "a",
        ELiteral(LString("asd")),
        ELet(
          "b",
          ELambda(
            "a",
            EApplication(
              EApplication(EVariable("+"), ELiteral(LInt(1))),
              EVariable("a")
            )
          ),
          EApplication(EVariable("b"), ELiteral(LInt(1)))
        )
      )
      val eff = runSynth(exp, ctx).flip
      assertM(eff)(Assertion.assertion("raises correct error")() {
        case it: AppError.ShadowedVariableName if it.name == "a" => true
        case _                                                   => false
      })
    },
    testM("args in other functions are not considered as shadowed") {
      val exp = ELet(
        "id",
        idFunction,
        ELet(
          "a",
          ELiteral(LInt(1)),
          ELiteral(LInt(1))
        )
      )
      assertM(runSynth(exp))(
        equalTo(TLiteral(LTInt))
      )
    },
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
        // TODO: nice_to_have: AppError.TypeNotKnown would be more informative
        Assertion.assertion("raises correct error")() {
          case it: AppError.TypeNotWellFormed
              if it._type == TTypeRef("annotatedType") =>
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
        equalTo(AppError.TypeNotApplicableToLiteral(LTBool, LInt(1)))
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
          TLambda(TTypeRef("asd"), TLiteral(LTBool)),
          ELet(
            "fun",
            EAnnotation(ELambda("a", ELiteral(LBool(false))), TTypeRef("kek")),
            EApplication(EVariable("fun"), ELiteral(LInt(1)))
          )
        )
      )
      assertM(runSynth(expr, ctx))(
        equalTo(TLiteral(LTBool))
      )
    }
  )
}
