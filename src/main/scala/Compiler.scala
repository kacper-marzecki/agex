import zio.*
import javax.xml.crypto.dsig.Transform
import cats.effect.concurrent.Supervisor.Token
import Statement.ModuleDefinition
import cats.implicits.*
import zio.interop.catz.*
class Compiler(
    listFiles: (path: String) => Eff[List[String]],
    getFile: (path: String) => Eff[String]
) {
  def compile(filePath: String): Eff[Unit] = {
    for {
      files   <- listFiles(filePath).flatMap(ZIO.foreach(_)(getFile))
      modules <- files.foldMapM(fileToModule)
      //  TODO NOW dependency graph between modules, deep-checking for references in AST
    } yield ()
  }

  def fileToModule(fileContent: String): Eff[List[ModuleDefinition]] =
    for {
      // file can contain multiple modules
      sexps <- ZIO
        .fromEither(Tokenizer.parseFileContent(fileContent))
        .mapError(AppError.ParserError(_))

      modules <- ZIO
        .fromEither(sexps.map(Transformer.toModule(_)).sequence)
        .mapError(AppError.AstTransformationError(_))
    } yield modules
}
