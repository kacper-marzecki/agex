import Expression.*
import Literal.*
import cats.data.NonEmptyList
import cats.implicits.*
import scala.runtime.stdLibPatches.language.experimental.namedTypeArguments
import cats.Eval
import cats.implicits.*
import ValueType.*
import Type.*
import TMapping.Required
import Statement.*

object Module {
  def addToContext(context: Context, eModule: ElixirModule): Eff[Context] = {
    val vars = eModule.members.collect {
      case member: ElixirFunction =>
        ContextElement.CTypedVariable(
          s"${eModule.name}.${member.name}",
          member._type
        )
      case member: ElixirTypeDef =>
        ContextElement.CTypeDefinition(
          s"${eModule.name}.${member.name}",
          member._type
        )
    }
    context.addAll(vars)
  }
}
