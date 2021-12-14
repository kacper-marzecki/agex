import zio.*
import javax.xml.crypto.dsig.Transform
import cats.effect.concurrent.Supervisor.Token
import Statement.*
import cats.implicits.*
import zio.interop.catz.*
import Expression.*
import TypedExpression.*
import Type.*
import scala.annotation.meta.field
import Pattern.*
case class ModuleDependencies(
    module: AgexModule,
    dependencies: List[String]
)

object Compiler {
  val stubCompiler = new Compiler(
    _ =>
      ZIO.succeed(
        List("asd")
      ),
    _ =>
      ZIO(
        scala.io.Source
          .fromFile("test.agex")
          .getLines
          .toList
          .foldSmash("", "\n", "")
      ).mapError(AppError.UnknownError(_))
  )
  def fileToModule(
      fileContent: String
  ): Eff[List[AgexModule]] =
    for {
      // file can contain multiple modules
      sexps <- ZIO
        .fromEither(Tokenizer.parseFileContent(fileContent))
        .mapError(AppError.ParserError(_))
        .tapError(pPrint(_, "PARSER ERR"))

      modules <- ZIO
        .foreach(sexps)(Sexp.toModule)
        .tapError(pPrint(_, "ASD"))
    } yield modules
}

class Compiler(
    listFiles: (path: String) => Eff[List[String]],
    getFile: (path: String) => Eff[String]
) {
  import Compiler.*
  val loadAgexCoreFiles = loadFilesInDirectory("agex")
  def compile(filePath: String): Eff[Unit] = {
    for {
      files           <- listFiles(filePath).flatMap(ZIO.foreach(_)(getFile))
      agesCoreFiles   <- loadAgexCoreFiles
      modules         <- (files ++ agesCoreFiles).foldMapM(fileToModule)
      agexCoreModules <- agesCoreFiles.foldMapM(fileToModule)
      _               <- validateUniqueModules(modules)
      existingModules = modules.map(_.name).toSet
      dependenciesAndModules <- getModuleDependencies(modules, existingModules)
      moduleToModulesDependingOnIt = dependenciesAndModules
        .flatMap(_.dependencies)
        .map { it =>
          (
            it,
            dependenciesAndModules
              .filter(_.dependencies.contains(it))
              .map(_.module.name)
              .toSet
          )
        }
        .toMap
      sortedModules <- ZIO
        .fromOption(
          Graph(moduleToModulesDependingOnIt).topologicalSort
        )
        .mapError(_ => AppError.ModuleCircularDependency())
        .map(_.filter(existingModules.contains))
      defaultContext <- ZIO.foldLeft(sortedModules)(Context()) {
        (c, moduleName) =>
          Module.addToGlobalContext(
            c,
            modules.find(_.name == moduleName).get
          )
      }
      defaultContextWithCoreModules <- ZIO.foldLeft(agexCoreModules)(
        defaultContext
      )(Module.addToLocalContext)
      // _ <- pPrint(defaultContext, "DEFAULT CONTEXT")
      // _ <- pPrint(sortedModules, "MODULES")

      // _ <- pPrint(
      // dependenciesAndModules,
      // "DEPENDENCIES AND MODULES"
      // )
      typedModules <- compile(
        sortedModules,
        modules,
        defaultContextWithCoreModules
      )
      _ <- pPrint(typedModules, "TYPED MODULES")
      _ <- ZIO.foreach(typedModules.map(ElixirOutput.toElixir))(
        pPrint(_, "MODULE")
      )
    } yield ()
  }.tapError(pPrint(_, "COMPILE ERROR"))

  case class State(modules: List[TypedModule], context: Context)
  def compile(
      sortedModules: List[String],
      modules: List[AgexModule],
      globalContext: Context
  ) = {
    ZIO
      .foldLeft(sortedModules)(State(Nil, globalContext)) { (s, moduleName) =>
        val module = modules.find(_.name == moduleName).get
        for {
          (typedModule, ctx) <- module match {
            case it: ModuleDefinition =>
              // Add aliased modules to the context as the alias
              val aliasedModules = it.aliases.map(moduleName =>
                modules.find(_.name == moduleName).get
              )
              for {
                aliasedContext <- ZIO.foldLeft(aliasedModules)(s.context)(
                  Module.addToAliasedContext
                )
                _ <- pPrint(it, "modele to alias")
                _ <- pPrint(aliasedContext, "ALIASED CONTEXT")
                result <- Module
                  .addToLocalContext(aliasedContext, it)
                  .flatMap { c =>
                    it.members.foldMapM {
                      case statement: Statement.ModuleAttribute =>
                        ZIO.succeed(List())
                      case statement: Statement.TypeDef => ZIO.succeed(List())
                      case statement: Statement.FunctionDef =>
                        synth(
                          EAnnotation(
                            EFunction(statement.args, statement.body),
                            statement._type
                          ),
                          c
                        ).map {
                          case (
                                gamma,
                                TEAnnotation(TEFunction(_, typed, _), _, _)
                              ) =>
                            println("statement._type")
                            println(statement._type)
                            List(
                              TypedStatement
                                .FunctionDef(
                                  statement.name,
                                  statement.args,
                                  typed
                                )
                            )
                          case _ => ???
                        }
                    }
                  }
                  .map(typedStatements =>
                    TypedModule(it.name, it.aliases, typedStatements)
                  )
                  .asSome
                  .tupleRight(aliasedContext)
              } yield result
            case it: ElixirModule =>
              val aliasedModules = it.aliases.map(moduleName =>
                modules.find(_.name == moduleName).get
              )
              for {
                aliasedContext <- ZIO.foldLeft(aliasedModules)(globalContext)(
                  Module.addToAliasedContext
                )
              } yield (None, aliasedContext)
          }
        } yield s.copy(
          context = ctx,
          modules = typedModule.toList ::: s.modules
        )
      }
      .map(_.modules)
  }

  def validateUniqueModules(modules: List[AgexModule]) = {
    val duplicateModules = modules
      .groupBy(_.name)
      .collect { case (moduleName, a :: b :: rest) =>
        moduleName
      }
      .toList
    if (duplicateModules.nonEmpty) {
      ZIO.fail(AppError.MultipleModuleDefinition(duplicateModules))
    } else {
      ZIO.unit
    }
  }

  def getModuleDependencies(
      modules: List[AgexModule],
      existingModules: Set[String]
  ) =
    ZIO.foreach(modules) { module =>
      val refs = module match {
        case it: ElixirModule     => getElixirModuleReferences(it)
        case it: ModuleDefinition => getModuleReferences(it)

      }
      ZIO
        .foreach(refs) { possibleRefs =>
          val hits = possibleRefs.filter(existingModules.contains(_))
          hits match {
            case List(hit) => ZIO.succeed(hit)
            case Nil       => ZIO.fail(AppError.ModulesNotFound(possibleRefs))
            case multiple =>
              ZIO.fail(
                AppError.AmbiguousModuleReference(possibleRefs, multiple)
              )
          }
        }
        .map(ModuleDependencies(module, _))
    }

  def getElixirModuleReferences(
      module: ElixirModule,
      existingAliases: Set[String] = Set()
  ): List[List[String]] = {
    val references =
      module.members.flatMap { member =>
        member match {
          case it: ElixirFunction => getModuleReferences(it._type)
          case it: ElixirTypeDef  => getModuleReferences(it._type)
        }
      }
    // match aliases with possible references from the aliased modules
    // (alias Phoenix.Controller
    //        Ecto.Changeset)
    // (def ... (Controller.json)
    // results in [[Phoenix.Controller, Controller]]
    references.map(ref =>
      ref ::
        module.aliases
          .flatMap(alias => fullModulePath(ref, alias).toList)
    )
  }

  def getModuleReferences(module: ModuleDefinition): List[List[String]] = {
    val references =
      module.members.flatMap { member =>
        member match {
          case it: FunctionDef     => getModuleReferences(it)
          case it: ModuleAttribute => getModuleReferences(it.body)
          case it: TypeDef         => getModuleReferences(it._type)
        }
      }
    references.map(ref =>
      ref ::
        module.aliases
          .flatMap(alias => fullModulePath(ref, alias).toList)
    )
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
      case ECase(exp, cases) =>
        cases.flatMap { case (m, e) =>
          getModuleReferences(m) ++ getModuleReferences(e)
        } ++ getModuleReferences(exp)
      case EIf(
            condition: Expression,
            ifTrue: Expression,
            ifFalse: Expression
          ) =>
        getModuleReferences(condition) ++ getModuleReferences(
          ifTrue
        ) ++ getModuleReferences(ifFalse)
    }

  def getModuleReferences(it: Pattern): List[String] = {
    it match {
      case PPin(exp) => getModuleReferences(exp)
      case _         => Nil
    }
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
      case TSum(xs) =>
        xs.flatMap(getModuleReferences).toList
      case TFunction(args: List[Type], ret: Type) =>
        args.flatMap(getModuleReferences) ++ getModuleReferences(ret)
      case TTypeApp(_type: Type, args: List[Type]) =>
        getModuleReferences(_type) ++ args.flatMap(getModuleReferences)
    }
  def getModuleReferences(it: Statement.FunctionDef): List[String] =
    getModuleReferences(it._type) ++ getModuleReferences(it.body)

  def fullModulePath(reference: String, alias: String) = {
    val aliasParts = alias.split('.')
    if (reference.startsWith(aliasParts.last)) {
      Some(
        aliasParts.reverse
          .drop(1)
          .reverse
          .toList
          .mkString(".") + "." + reference
      )
    } else None
  }

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
