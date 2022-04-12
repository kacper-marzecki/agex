import zio._

case class CompilerState(existentialCounter: Ref[Int]) {
  def makeExistential: UIO[String] = {
    for {
      counter <- existentialCounter.updateAndGet(_ + 1)
    } yield s"t$counter"
  }
}

object CompilerState {
  def live =
    ZLayer.fromEffect {
      Ref.make(0).map { it => CompilerState(it) }
    }

  def makeExistential: URIO[Has[CompilerState], String] =
    ZIO.accessM(_.get.makeExistential)
}
