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
  def addToGlobalContext(
      context: Context,
      module: AgexModule
  ): Eff[Context] =
    module match {
      case module: ElixirModule =>
        addToContext(context, module, it => s"${it.name}.")
      case module: ModuleDefinition =>
        addToContext(context, module, it => s"${it.name}.")
    }

  def addToLocalContext(
      context: Context,
      module: AgexModule
  ): Eff[Context] =
    module match {
      case module: ElixirModule =>
        addToContext(context, module, _ => "")
      case module: ModuleDefinition =>
        addToContext(context, module, _ => "")
    }

  def addToContext(
      context: Context,
      eModule: ElixirModule,
      prefixFn: AgexModule => String
  ): Eff[Context] = {
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

  def addToContext(
      context: Context,
      module: ModuleDefinition,
      prefixFn: AgexModule => String
  ): Eff[Context] = {
    val vars = module.members.collect {
      case member: FunctionDef =>
        ContextElement.CTypedVariable(
          s"${module.name}.${member.name}",
          member._type
        )
      case member: TypeDef =>
        ContextElement.CTypeDefinition(
          s"${module.name}.${member.name}",
          member._type
        )
    }
    context.addAll(vars)
  }
}
