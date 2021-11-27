import zio.*

object DefaultContext {
  // val get: Eff[Context] =
  //   for {
  //     kernelModules <- loadElixirInterfaceModule("agex/Kernel.agex")
  //     ctx <- ZIO.foldLeft(kernelModules)(
  //       Context(
  //         Vector()
  //       )
  //     ) { (ctx, module) =>
  //       Module.addToContext(ctx, module)
  //     }
  //   } yield ctx

  def loadElixirInterfaceModule(fileName: String) = {
    for {
      content <- loadFile(fileName)
      modules <- Compiler
        .fileToModule(content)
        .map(it => it.collect { case it: ElixirModule => it })
    } yield modules
  }
}
