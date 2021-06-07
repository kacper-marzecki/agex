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
  case Unexpected(throwable: Throwable)
  case TypeNotApplicableToLiteral(_type: LiteralType, literal: Literal)
  case ElementNotFound(context: Context, element: ContextElement)
  case AnnotationNotFound(context: Context, name: String)
  case TypeNotWellFormed(context: Context, _type: Type)
  case CannotApplyType(_type: Type)
  case TypesNotEqual(_typeA: Type, _typeB: Type)
  case TypeNamesNotEqual(alpha1: String, alpha2: String)
  case CircularInstantiation(context: Context, name: String, _type: Type)
  case CannotInstantiate(context: Context, name: String, _type: Type)
}

type Env = ZEnv with Has[MutableState]
type Eff = [A] =>> ZIO[Env, AppError, A]
