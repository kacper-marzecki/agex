import Expression._
import Literal._
import cats.data.NonEmptyList
import cats.implicits._
import cats.Eval
import cats.implicits._
import ValueType._
import Type._
import TMapping.Required
import Statement._
import Eff._
import Utils._
object Module {
  // TODO these names are awful

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

  def addToAliasedContext(
      context: Context,
      module: AgexModule
  ): Eff[Context] = {
    val f = (module: AgexModule) => module.name.split('.').last + "."
    module match {
      case module: ElixirModule =>
        addToContext(context, module, f)
      case module: ModuleDefinition =>
        addToContext(context, module, f)
    }
  }

  def addToContext(
      context: Context,
      eModule: ElixirModule,
      prefixFn: AgexModule => String
  ): Eff[Context] = {
    val vars = eModule.members.collect {
      case member: ElixirFunction =>
        ContextElement.CTypedVariable(
          s"${prefixFn(eModule)}${member.name}",
          qualifyLocalRef(eModule.name, member._type),
          isLocal = false,
          infix = member.infix
        )
      case member: ElixirTypeDef =>
        ContextElement.CTypeDefinition(
          s"${prefixFn(eModule)}${member.name}",
          qualifyLocalRef(eModule.name, member._type)
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
          s"${prefixFn(module)}${member.name}",
          qualifyLocalRef(module.name, member._type),
          false
        )
      case member: TypeDef =>
        ContextElement.CTypeDefinition(
          s"${prefixFn(module)}${member.name}",
          qualifyLocalRef(module.name, member._type)
        )
    }
    context.addAll(vars)
  }
}
