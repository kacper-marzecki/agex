import zio.*
import javax.xml.crypto.dsig.Transform
import cats.effect.concurrent.Supervisor.Token
import Statement.*
import cats.implicits.*
import zio.interop.catz.*
import Expression.*
import Type.*
import scala.annotation.meta.field

case class ModuleDependencies(
    module: ModuleDefinition,
    dependencies: List[String]
)

object Compiler {
  val a = new Compiler(
    _ =>
      ZIO.succeed(
        List("asd")
      ),
    _ => ZIO.succeed("""
  (defmodule Ecto.Changeset)
  (defmodule Kek
    (alias Ecto.Changeset)
    (defn increment 
          ([Integer] Integer) 
          [a] (Math.plus a amount)))
  (defmodule Math
    (defn plus
      ([Integer, Integer] Integer)
      [a, b] (+ a b)))
  """)
  )
  def fileToModule(fileContent: String): Eff[List[ModuleDefinition]] =
    for {
      // file can contain multiple modules
      sexps <- ZIO
        .fromEither(Tokenizer.parseFileContent(fileContent))
        .mapError(AppError.ParserError(_))
        .tapError(pPrint(_, "PARSER ERR"))

      modules <- ZIO
        .fromEither(sexps.map(Transformer.toModule(_)).sequence)
        .mapError(AppError.AstTransformationError(_))
    } yield modules
}

class Compiler(
    listFiles: (path: String) => Eff[List[String]],
    getFile: (path: String) => Eff[String]
) {
  import Compiler.*
  def compile(filePath: String): Eff[Unit] = {
    for {
      files   <- listFiles(filePath).flatMap(ZIO.foreach(_)(getFile))
      modules <- files.foldMapM(fileToModule)
      dependenciesAndModules <- getModuleDependencies(modules)
      existingModules = dependenciesAndModules.map(_.module.name).toSet

      sortedModules <- ZIO
        .fromOption(
          Graph(
            dependenciesAndModules
              .map(a => (a.module.name, a.dependencies.toSet))
              .toMap
          ).topologicalSort
        )
        .mapError(_ => AppError.ModuleCircularDependency())
        .map(_.filter(existingModules.contains).reverse)
      _ <- pPrint(sortedModules, "MODULES")

      _ <- pPrint(
        dependenciesAndModules.map(_.dependencies),
        "DEPENDENCIES AND MODULES"
      )

      // List(List("Math", "Ecto.Changeset.Math", "Ecto.Changeset"), List())
      // ^ remove non-existing modules from dependencies
      // raise on conflicting modules

    } yield ()
  }

  def getModuleDependencies(modules: List[ModuleDefinition]) = {
    val existingModules = modules.map(_.name).toSet

    ZIO.foreach(modules) { module =>
      val refs            = getModuleReferences(module)
      val nonExistingRefs = refs.filter(!existingModules.contains(_))
      if (nonExistingRefs.isEmpty) {
        ZIO.succeed(ModuleDependencies(module, refs))
      } else
        ZIO.fail(AppError.ModulesNotFound(nonExistingRefs))
    }
  }

  def getModuleReferences(
      module: ModuleDefinition,
      existingAliases: Set[String] = Set()
  ): List[String] = {
    val (references, aliases) =
      module.members.foldLeft((Set[String](), existingAliases)) {
        case ((references, aliases), member) =>
          member match {
            case it: FunctionDef =>
              (
                references ++ withAliases(getModuleReferences(it), aliases),
                aliases
              )
            case it: ModuleAttribute =>
              (
                references ++ withAliases(
                  getModuleReferences(it.body),
                  aliases
                ),
                aliases
              )
            case it: Alias =>
              (
                references,
                aliases + it.moduleName ++ aliases.map(_ + "." + it.moduleName)
              )
            case it: TypeDef =>
              (
                references ++ withAliases(
                  getModuleReferences(it._type),
                  aliases
                ),
                aliases
              )
          }
      }
    references.combine(aliases).toList
  }

  private def withAliases(references: List[String], aliases: Set[String]) =
    // references ++ aliases.flatMap(alias => references.map(alias + "." + _))
    references ++ aliases

  def getModuleReferences(expression: Expression): List[String] =
    expression match {
      case EVariable(name: String) => getModuleReference(name).toList
      case ELiteral(it: Literal)   => Nil
      case ELet(name: String, value: Expression, body: Expression) =>
        getModuleReferences(value) ++ getModuleReferences(body)

      case EAnnotation(expr: Expression, annotatedType: Type) =>
        getModuleReferences(expr) ++ getModuleReferences(annotatedType)

      case ETuple(values: List[Expression]) =>
        values.flatMap(getModuleReferences)
      case ETypeAlias(newName: String, targetType: Type, expr: Expression) =>
        getModuleReferences(targetType) ++ getModuleReferences(expr)

      case EStruct(fields: Map[String, Expression]) =>
        fields.values.toList.flatMap(getModuleReferences)
      case EMap(kvs: List[(Expression, Expression)]) =>
        kvs.flatMap({ case (k, v) =>
          getModuleReferences(k) ++ getModuleReferences(v)
        })
      case EList(values: List[Expression]) =>
        values.flatMap(getModuleReferences)
      case EFunction(args: List[String], body: Expression) =>
        getModuleReferences(body)
      case EFunctionApplication(fun: Expression, args: List[Expression]) =>
        getModuleReferences(fun) ++ args.flatMap(getModuleReferences)
      case EIf(
            condition: Expression,
            ifTrue: Expression,
            ifFalse: Expression
          ) =>
        getModuleReferences(condition) ++ getModuleReferences(
          ifTrue
        ) ++ getModuleReferences(ifFalse)

    }
  def getModuleReferences(it: Type): List[String] =
    it match {
      case TAny                               => Nil
      case TNothing                           => Nil
      case TValue(valueType: ValueType)       => Nil
      case TLiteral(literalType: LiteralType) => Nil
      case TVariable(name: String)            => getModuleReference(name).toList
      case TExistential(name: String)         => Nil
      case TQuantification(name: String, _type: Type) =>
        getModuleReferences(_type)
      case TMulQuantification(names: List[String], _type: Type) =>
        getModuleReferences(_type)
      case TTuple(valueTypes: List[Type]) =>
        valueTypes.flatMap(getModuleReferences)
      case TTypeRef(targetType: String) => getModuleReference(targetType).toList
      case TStruct(fieldTypes: Map[String, Type]) =>
        fieldTypes.values.toList.flatMap(getModuleReferences)
      case TList(valueType: Type) => getModuleReferences(valueType)
      case TMap(kvs: List[TMapping]) =>
        kvs.flatMap(it =>
          getModuleReferences(it.k) ++ getModuleReferences(it.v)
        )
      case TSum(a: Type, b: Type) =>
        getModuleReferences(b) ++ getModuleReferences(a)

      case TFunction(args: List[Type], ret: Type) =>
        args.flatMap(getModuleReferences) ++ getModuleReferences(ret)
      case TTypeApp(_type: Type, args: List[Type]) =>
        getModuleReferences(_type) ++ args.flatMap(getModuleReferences)
    }
  def getModuleReferences(it: Statement.FunctionDef): List[String] =
    getModuleReferences(it._type) ++ getModuleReferences(it.body)

  def getModuleReference(string: String) = {
    if (string.split('.').length > 1) {
      Some(
        string.split('.').reverse.drop(1).reverse.toList.foldSmash("", ".", "")
      )
    } else None
  }

}

case class Graph[A](adj: Map[A, Set[A]]) {
  case class DfsState(
      discovered: Set[A] = Set(),
      activeNodes: Set[A] = Set(),
      tsOrder: List[A] = List(),
      isCylic: Boolean = false
  )

  def dfs: (List[A], Boolean) = {
    def dfsVisit(currState: DfsState, src: A): DfsState = {
      val newState = currState.copy(
        discovered = currState.discovered + src,
        activeNodes = currState.activeNodes + src,
        isCylic = currState.isCylic || adj
          .get(src)
          .getOrElse(Set())
          .exists(currState.activeNodes)
      )

      val finalState = adj
        .get(src)
        .getOrElse(Set())
        .filterNot(newState.discovered)
        .foldLeft(newState)(dfsVisit(_, _))
      finalState.copy(
        tsOrder = src :: finalState.tsOrder,
        activeNodes = finalState.activeNodes - src
      )
    }

    val stateAfterSearch = adj.keys.foldLeft(DfsState()) { (state, n) =>
      if (state.discovered(n)) state else dfsVisit(state, n)
    }
    (stateAfterSearch.tsOrder, stateAfterSearch.isCylic)
  }

  // TODO return Either[Set[A], List[A]]
  //                     ^          ^
  //                     |          |
  //                   cycle      result
  def topologicalSort: Option[List[A]] = dfs match {
    case (topologicalOrder, false) => Some(topologicalOrder)
    case _                         => None
  }
}
