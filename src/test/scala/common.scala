import Expression.*
import Type.*
import LiteralType.*
import Literal.*
import ContextElement.*

object TestCommonExpressions {
  val litInt = ELiteral(LInt(1))
  val litBool = ELiteral(LBool(false))
  val litString = ELiteral(LString("string"))
  val idFunction = ELambda("x", EVariable("x"))
  val annotatedId = EAnnotation(
    idFunction,
    TQuantification("a", TFunction(TVariable("a"), TVariable("a")))
  )
  val strBoolTuple = ETuple(List(litString, litBool))
}

object CommonTestFunctions {
  def runSynth(expr: Expression, context: Context = Context()) =
    synth(expr, context)
      // .tap(prettyPrint(_, "synthResult"))
      .map(_._type)
      .provideSomeLayer[zio.ZEnv](CompilerState.live)
  // .tapError(prettyPrint(_, "synthError"))

}
