import zio.*

enum AppError extends RuntimeException {
  case TypeNotApplicableToLiteral(_type: Type, literal: Literal)
  case ElementNotFound(context: Context, element: ContextElement)
  case AnnotationNotFound(context: Context, name: String)
  case TypeNotWellFormed(context: Context, _type: Type)
  case CannotApplyType(_type: Type)
  case TypesNotEqual(_typeA: Type, _typeB: Type)
  case TypeNamesNotEqual(alpha1: String, alpha2: String)
  case CircularInstantiation(context: Context, name: String, _type: Type)
  case CannotSubtype(context: Context, a: Type, b: Type)
  case CannotInstantiateR(context: Context, name: String, _type: Type)
  case CannotInstantiateL(context: Context, name: String, _type: Type)
  case ShadowedVariableName(context: Context, name: String)
  case TypeWithNameAlreadyExists(context: Context, name: String, _type: Type)
  case TypeNotKnown(context: Context, name: String)
  case MissingFields(fields: List[String])
  case TupleSizesDontMatch(a: Type.TTuple, b: Type.TTuple)
  case Unexpected(label: String)
  case WrongArity(expected: Int, actual: Int)
  case UnknownError(throwable: Throwable)
  case ParserError(error: cats.parse.Parser.Error)
  case AstTransformationError(error: String)
  // TODO add more info
  case ModuleCircularDependency()
  case ModulesNotFound(modules: List[String])
}

type Env = ZEnv & Has[CompilerState]
type Eff = [A] =>> ZIO[Env, AppError, A]
