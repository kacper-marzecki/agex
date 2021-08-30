import Expression.*
import Type.*
import LiteralType.*
import Literal.*
import ContextElement.*
import zio.*

object TestCommonExpressions {
  val litInt     = ELiteral(LInt(1))
  val litBool    = ELiteral(LBool(false))
  val litString  = ELiteral(LString("string"))
  val idFunction = EFunction(List("x"), EVariable("x"))
  val annotatedId = EAnnotation(
    idFunction,
    TMulQuantification(
      List("a"),
      TFunction(List(TVariable("a")), TVariable("a"))
    )
  )
  val strBoolTuple = ETuple(List(litString, litBool))
}

object CommonTestFunctions {
  def runSynthDebug(
      expr: Expression,
      context: Context = Context()
  ) = runSynth(expr, context, true)

  def runSynth(
      expr: Expression,
      context: Context = Context(),
      debug: Boolean = false
  ) =
    synth(expr, context)
      .tap(if (debug) prettyPrint(_, "synthResult") else _ => ZIO.unit)
      .map(_._2)
      .map(_._type)
      .provideSomeLayer[zio.ZEnv](CompilerState.live)
      .tapError(if (debug) prettyPrint(_, "synthError") else _ => ZIO.unit)

  def stringToExpr(str: String) = {
    for {
      sexpr <- ZIO.fromEither(Tokenizer.pExpr.parseAll(str))
      ast   <- ZIO.fromEither(Transformer.toAst(sexpr))
    } yield ast
  }

  def parseAndSynth(str: String) = {
    for {
      expr    <- stringToExpr(str)
      synthed <- runSynth(expr)
    } yield (expr, synthed)
  }
}
