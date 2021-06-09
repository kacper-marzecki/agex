import zio.*

case class MutableState(existentialCounter: Ref[Int]) {
  def makeExistential: UIO[String] = {
    for {
      counter <- existentialCounter.updateAndGet(_ + 1)
    } yield s"t$counter"
  }
}

object MutableState {
  def live =
    ZLayer.fromEffect {
      Ref.make(0).map { it => MutableState(it) }
    }

  def makeExistential: URIO[Has[MutableState], String] =
    ZIO.accessM(_.get.makeExistential)
}

enum AppError extends RuntimeException {
  def show =
    this match {
      case TypeNotApplicableToLiteral(_type: LiteralType, literal: Literal) =>
        s"TypeNotApplicableToLiteral: ${_type}, $literal"
      case ElementNotFound(context: Context, element: ContextElement) =>
        s"ElementNotFound: $element in context: \n$context"
      case AnnotationNotFound(context: Context, name: String) =>
        s"AnnotationNotFound: $name in context: \n$context"
      case TypeNotWellFormed(context: Context, _type: Type) =>
        s"TypeNotWellFormed: ${_type} in context: \n$context"
      case CannotApplyType(_type: Type) => s"CannotApplyType: ${_type}"
      case TypesNotEqual(_typeA: Type, _typeB: Type) =>
        s"TypesNotEqual: ${_typeA} ${_typeB}"
      case TypeNamesNotEqual(alpha1: String, alpha2: String) =>
        s"TypeNamesNotEqual: $alpha1 $alpha2"
      case CircularInstantiation(context: Context, name: String, _type: Type) =>
        s"CircularInstantiation: $name ${_type} in context: \n$context"
      case CannotInstantiateR(context: Context, name: String, _type: Type) =>
        s"CannotInstantiateR:  $name ${_type} in context: \n$context"
      case CannotInstantiateL(context: Context, name: String, _type: Type) =>
        s"CannotInstantiateL:  $name ${_type} in context: \n$context"
      case CannotSubtype(context: Context, a: Type, b: Type) =>
        s"CannotSubtype: $a $b in context: \n$context"

    }
  case TypeNotApplicableToLiteral(_type: LiteralType, literal: Literal)
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
}

type Env = ZEnv & Has[MutableState]
type Eff = [A] =>> ZIO[Env, AppError, A]
