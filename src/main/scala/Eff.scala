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
}

enum AppError {
  case Unexpected(throwable: Throwable)
  case TypeNotApplicableToLiteral(_type: LiteralType, literal: Literal)
}

type Env = ZEnv with Has[MutableState]
type Eff = [A] =>> ZIO[Env, AppError, A]
