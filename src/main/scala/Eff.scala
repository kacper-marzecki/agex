import zio._

sealed trait AppError extends RuntimeException

object AppError {
  case class TypeNotApplicableToLiteral(_type: Type, literal: Literal)
      extends AppError
  case class TypeNotApplicableToExpression(_type: Type, expression: Expression)
      extends AppError
  case class ElementNotFound(context: Context, element: ContextElement)
      extends AppError
  case class AnnotationNotFound(context: Context, name: String) extends AppError
  case class TypeNotWellFormed(context: Context, _type: Type)   extends AppError
  case class CannotApplyType(_type: Type)                       extends AppError
  case class TypesNotEqual(_typeA: Type, _typeB: Type)          extends AppError
  case class TypeNamesNotEqual(alpha1: String, alpha2: String)  extends AppError
  case class CircularInstantiation(context: Context, name: String, _type: Type)
      extends AppError
  case class CannotSubtype(context: Context, a: Type, b: Type) extends AppError
  case class CannotInstantiateR(context: Context, name: String, _type: Type)
      extends AppError
  case class CannotInstantiateL(context: Context, name: String, _type: Type)
      extends AppError
  case class ShadowedVariableName(context: Context, name: String)
      extends AppError
  case class TypeWithNameAlreadyExists(
      context: Context,
      name: String,
      _type: Type
  )                                                       extends AppError
  case class TypeNotKnown(context: Context, name: String) extends AppError
  case class MissingFields(fields: List[String])          extends AppError
  case class TupleSizesDontMatch(a: Type.TTuple, b: Type.TTuple)
      extends AppError
  case class Unexpected(label: String)                         extends AppError
  case class WrongArity(expected: Int, actual: Int)            extends AppError
  case class PatternDoesntMatch(pattern: Pattern, _type: Type) extends AppError
  case class InvalidPattern(description: String)               extends AppError
  case class UnknownError(throwable: Throwable)                extends AppError
  case class ParserError(error: cats.parse.Parser.Error)       extends AppError
  case class AstTransformationError(error: String)             extends AppError
  // TODO add more info
  case class ModuleCircularDependency()                      extends AppError
  case class ModulesNotFound(modules: List[String])          extends AppError
  case class MultipleModuleDefinition(modules: List[String]) extends AppError
  case class AmbiguousModuleReference(
      references: List[String],
      possibleModules: List[String]
  ) extends AppError
  case class CompilationError(expression: Expression, error: AppError)
      extends AppError
}

object Eff {
  type Env    = ZEnv & Has[CompilerState]
  type Eff[A] = ZIO[Env, AppError, A]
}
