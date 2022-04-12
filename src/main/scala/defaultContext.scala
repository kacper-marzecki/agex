import zio._
import Utils._
object DefaultContext {
  def loadElixirInterfaceModule(fileName: String) = {
    for {
      content <- loadFile(fileName)
      modules <- Compiler
        .fileToModule(content)
        .map(it => it.collect { case it: ElixirModule => it })
    } yield modules
  }
}
