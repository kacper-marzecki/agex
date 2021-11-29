import Expression.*
import Literal.*
import cats.parse.{Parser0 as P0, Parser as P, Numbers}
import cats.data.NonEmptyList
import cats.implicits.*
import scala.runtime.stdLibPatches.language.experimental.namedTypeArguments
import cats.Eval
import cats.implicits.*
import ValueType.*
import Type.*
import TMapping.Required
import Statement.*
import LiteralType.*
import scala.util.Random

object Sexp {
  def toAstList(exprsList: List[SExp]): Either[String, List[Expression]] =
    exprsList.foldMapM(toAst(_).map(List(_)))

  def toAst(expr: SExp): Either[String, Expression] = {
    expr match {
      case SId(it) =>
        parseId(it)
      case SString(it) => Right(ELiteral(LString(it)))
      case SList(xs)   => sexp(xs)
      case SSquareList(xs) =>
        toAstList(xs).map(EList(_))
      case SCurlyList(xs)  => toAstList(xs).map(ETuple(_))
      case SMapLiteral(xs) => parseMap(xs)
      // TODO missing structs
      // transforming into a struct
      // if (xs.size % 2 != 0) Left("not even struct elements")
      // else {
      //   val a = xs
      //     .sliding(2)
      //     .toList
      //     .foldMapM {
      //       case k :: v :: Nil =>
      //         for {
      //           l <- requireId(k).map(_.value)
      //           r <- toAst(v)
      //         } yield List((l, r))
      //       case _ => Left("Should not occur")
      //     }
      //     .map(pairs => EStruct(pairs.toMap))
      // }
    }
  }

  // Here we're handling special forms, and in the future I guess this would be the place to expand macros
  // TODO think out: macros
  def sexp(exprs: List[SExp]) = exprs match {
    case Nil                 => Right(ELiteral(LUnit))
    case SId("fn") :: xs     => parseArgsAndBody(xs)
    case SId(":") :: xs      => parseTypeAnnotation(xs)
    case SId("let") :: xs    => parseLet(xs)
    case SId("type") :: xs   => parseTypeAlias(xs)
    case SId("if") :: xs     => parseIf(xs)
    case functionApplication => parseFunctionApplication(functionApplication)
  }

  def toModule(
      expr: SExp
  ): Either[String, AgexModule] = {
    expr match {
      case SList(
            SId("defmodule") :: SId(moduleName) :: SList(
              SId("alias") :: aliasList
            ) :: moduleMembers
          ) =>
        getAliases(aliasList).flatMap(aliases =>
          moduleMembers
            .foldMapM(toStatement(_).map(List(_)))
            .map(it => ModuleDefinition(moduleName, aliases, it))
        )
      case SList(SId("defmodule") :: SId(moduleName) :: moduleMembers) =>
        moduleMembers
          .foldMapM(toStatement(_).map(List(_)))
          .map(it => ModuleDefinition(moduleName, List(), it))
      case SList(
            SId("defelixir") :: SId(moduleName) :: SList(
              SId("alias") :: aliasList
            ) :: moduleMembers
          ) =>
        getAliases(aliasList).flatMap(aliases =>
          moduleMembers
            .foldMapM(toElixirModuleStatement(_).map(List(_)))
            .map(it => ElixirModule(moduleName, aliases, it))
        )
      case SList(SId("defelixir") :: SId(moduleName) :: moduleMembers) =>
        moduleMembers
          .foldMapM(toElixirModuleStatement(_).map(List(_)))
          .map(it => ElixirModule(moduleName, List(), it))
      case _ => Left(s"Expected module definition, got: $expr")
    }
  }

  def getAliases(aliases: List[SExp]) =
    aliases.foldMapM {
      case SId(name) => Right(List(name))
      case _         => Left("Only module names allowed in aliases ")
    }

  def toElixirModuleStatement(
      expr: SExp
  ): Either[String, ElixirModuleStatement] =
    expr match {
      case SList(
            SId("def") :: SId(functionName) :: SList(
              List(SSquareList(argTypes), returnType)
            ) :: SString(elixirFunctionName) :: Nil
          ) =>
        for {
          _type <- parseType(
            SList(List(SId("fn"), SSquareList(argTypes), returnType))
          )
        } yield ElixirFunction(
          functionName,
          _type,
          elixirFunctionName
        )
      case SList(SId("deftype") :: SId(name) :: _type :: Nil) =>
        parseType(_type).map(ElixirTypeDef(name, _))
      case _ => Left(s"Unrecognized Elixir interface module construct: $expr")
    }

  def requireId(expr: SExp) = expr match {
    case it: SId => Right(it)
    case _       => Left("required Id")
  }

  def parseArgsAndBody(xs: List[SExp]) =
    xs match {
      case SSquareList(argSexprs) :: body :: Nil =>
        for {
          args     <- argSexprs.foldMapM(requireId(_).map(List(_)))
          bodyExpr <- toAst(body)
        } yield EFunction(args.map(_.value), bodyExpr)
      case _ =>
        Left("structure of an anonymous function: `(fn [arg1 arg2] (body))`")
    }

  def parseIdType(str: String) = {
    str match {
      case "integer" => Right(TLiteral(LTInt))
      case "float"   => Right(TLiteral(LTFloat))
      case "boolean" => Right(TLiteral(LTBool))
      case "string"  => Right(TLiteral(LTString))
      case "char"    => Right(TLiteral(LTChar))
      case "any"     => Right(TAny)
      case str if str.startsWith(":") =>
        Right(TValue(VTAtom(str.substring(1))))
      case "()"  => Right(TValue(VTUnit))
      case "nil" => Right(TValue(VTNil))
      case _ =>
        List(
          str.toIntOption.map(it => TValue(VTInt(it))),
          str.toBooleanOption.map(it => TValue(VTBool(it))),
          str.toFloatOption.map(it => TValue(VTFloat(it)))
        ).find(_.isDefined)
          .flatten
          .fold(Right(TTypeRef((str))))(Right(_))
    }
  }

  def toStatement(expr: SExp): Either[String, Statement] =
    expr match {
      // TODO match on statement type and check proper structure in other functions
      case SList(
            SId("defn") :: SId(functionName) :: SList(
              List(SSquareList(argTypes), returnType)
            ) :: SSquareList(
              args
            ) :: functionBody :: Nil
          ) =>
        for {
          _type <- parseType(
            SList(List(SId("fn"), SSquareList(argTypes), returnType))
          )
          body <- toAst(functionBody)
          argumentNames <- args.foldMapM {
            case SId(name) => Right(List(name))
            case other     => Left(s"Expected argument name, got: $other")
          }
        } yield FunctionDef(
          functionName,
          _type,
          argumentNames,
          body
        )
      case SList(SId("def") :: SId(attributeName) :: attributeBody :: Nil) =>
        toAst(attributeBody).map(ModuleAttribute(attributeName, _))
      case SList(SId("deftype") :: SId(name) :: _type :: Nil) =>
        parseType(_type).map(TypeDef(name, _))
      case _ => Left(s"Unrecognized top-level construct: $expr")
    }
  def parseId(str: String) = {
    str match {
      case str if str.startsWith(":") =>
        Right(ELiteral(LAtom(str.substring(1))))
      case "()"  => Right(ELiteral(LUnit))
      case "nil" => Right(ELiteral(LNil))
      case _ =>
        List(
          str.toIntOption.map(it => ELiteral(LInt(it))),
          str.toBooleanOption.map(it => ELiteral(LBool(it))),
          str.toFloatOption.map(it => ELiteral(LFloat(it)))
        ).find(_.isDefined)
          .flatten
          .fold(Right(EVariable((str))))(Right(_))
    }
  }

  def parseMap(xs: List[SExp]) =
    xs.foldMapM(toAst(_).map(List(_)))
      .flatMap(paired)
      .map(EMap(_))

  def parseTypeAnnotation(xs: List[SExp]) =
    xs match {
      case annotatedTypeSexp :: expressionSexp :: Nil =>
        for {
          parsedType       <- parseType(annotatedTypeSexp)
          parsedExpression <- toAst(expressionSexp)
        } yield EAnnotation(parsedExpression, parsedType)
      case _ => Left("structure of a type annotation:  `(: type expression)`")
    }

  def parseIf(xs: List[SExp]) =
    xs match {
      case cond :: ifTrue :: ifFalse :: Nil =>
        for {
          condE    <- toAst(cond)
          ifTrueE  <- toAst(ifTrue)
          ifFalseE <- toAst(ifFalse)
        } yield EIf(condE, ifTrueE, ifFalseE)
      case _ =>
        Left("Structure of an if expression: (if condition if-true if-false)")
    }

  def parseLet(xs: List[SExp]) =
    xs match {
      case SSquareList(bindings) :: body :: Nil =>
        for {
          nameExpressionPairs_? <- bindings
            .sliding(2, 2)
            .toList
            .foldMapM(parseLetBinding(_).map(List(_)))
          nameExpressionPairs <- nameExpressionPairs_? match {
            case first :: rest => Right(NonEmptyList(first, rest))
            case _ =>
              Left("at least one let binding is required in the let form ")
          }
          bodyExpr <- toAst(body)
        } yield nameExpressionPairs.toList.foldRight(bodyExpr) {
          case ((name, binding), acc) => ELet(name, binding, acc)
        }
      case _ =>
        Left(
          s"structure of a type annotation:  `(: type expression)`, found: ${xs}"
        )
    }

  def parseLetBinding(xs: List[SExp]): Either[String, (String, Expression)] =
    xs match {
      case SId(name) :: body :: Nil =>
        toAst(body).map((name, _))
      case a => Left("structure of a let binding:  `name expression`")
    }

  def parseFunctionApplication(
      xs: List[SExp]
  ): Either[String, EFunctionApplication] =
    xs match {
      case function :: args =>
        for {
          functionE <- toAst(function)
          argsE     <- toAstList(args)
        } yield EFunctionApplication(functionE, argsE)
      case _ =>
        Left("structure of a function application: (function-name arg1 arg2)")
    }

  def parseTypeAlias(xs: List[SExp]): Either[String, Expression] =
    xs match {
      case SId(name) :: typeDef :: body :: Nil =>
        for {
          parsedTypeDef <- parseType(typeDef)
          parsedBody    <- toAst(body)
        } yield ETypeAlias(name, parsedTypeDef, parsedBody)
      case _ =>
        Left(
          "structure of a type alias:  `(type  newName someType in-expression`"
        )
    }

  def parseType(sexp: SExp): Either[String, Type] = sexp match {
    case SId(value)     => parseIdType(value)
    case SString(value) => Right(TValue(VTString(value)))
    case SList(elements) =>
      elements match {
        case SId("|") :: sum1 :: sum2 :: sums =>
          for {
            sum1T <- parseType(sum1)
            sum2T <- parseType(sum2)
            sumsT <- sums.map(parseType).sequence
          } yield sumsT.foldLeft(TSum(sum1T, sum2T)) { case (agg, x) =>
            TSum(x, agg)
          }
        case Nil => Right(TValue(VTUnit))
        case SId("fn") :: SSquareList(args) :: returnType :: Nil => {
          for {
            retT  <- parseType(returnType)
            argsT <- args.map(parseType).sequence
          } yield TFunction(argsT, retT)
        }
        case SId("forall") :: SSquareList(
              typeArgs
            ) :: quantifiedType :: Nil => {
          for {
            quantT <- parseType(quantifiedType)
            args   <- typeArgs.map(requireId).sequence
            // _ = quantT
          } yield replaceTypeRefsWithTVars(
            TMulQuantification(args.map(_.value), quantT)
          )
        }
        case _type :: typeArguments =>
          for {
            typedType     <- parseType(_type)
            typedTypeArgs <- typeArguments.map(parseType).sequence
          } yield TTypeApp(typedType, typedTypeArgs)
      }
    case SSquareList(elements) =>
      elements match {
        case x :: Nil => parseType(x).map(TList(_))
        case Nil      => Right(TList(TNothing))
        case _        => Left("Structure of a List type : [] | [TypeName]")
      }
    case SCurlyList(elements) => elements.map(parseType).sequence.map(TTuple(_))
    case SMapLiteral(elements) =>
      elements
        .map(parseType)
        .sequence
        .flatMap(paired)
        .map(pairs => TMap(pairs.map(Required.apply.tupled)))
  }
  // TODO impure
  var a = 0
  def replaceTypeRefsWithTVars(
      t: Type,
      priorBindings: Map[String, String] = Map()
  ): Type = {
    val f = replaceTypeRefsWithTVars(_, priorBindings)
    t match {
      case TList(valueType) => f(valueType)
      case TFunction(argTypes, returnType) =>
        TFunction(argTypes.map(f), f(returnType))
      case it: TMulQuantification =>
        a += 1
        val x        = a
        val newNames = it.names.map(a => (a, s"$a$x")).toMap
        val nameMap  = priorBindings ++ newNames
        TMulQuantification(
          it.names.map(newNames),
          replaceTypeRefsWithTVars(it._type, nameMap)
        )
      case TSum(x, y) => TSum(f(x), f(y))
      case TTypeApp(quant, args) =>
        TTypeApp(
          replaceTypeRefsWithTVars(quant, priorBindings),
          args.map(replaceTypeRefsWithTVars(_, priorBindings))
        )
      case TTuple(valueTypes) => TTuple(valueTypes.map(f))
      case TVariable(name)    => TVariable(priorBindings(name))
      case TTypeRef(name) =>
        priorBindings
          .get(name)
          .fold(TTypeRef(name))(TVariable(_))
      case TMap(mappings) =>
        TMap(mappings.map {
          case TMapping.Required(k, v) => TMapping.Required(f(k), f(v))
          case TMapping.Optional(k, v) => TMapping.Optional(f(k), f(v))
        })
      case other => other
    }
  }
  // TODO should probably be specific to SExp to be able to include line info in the error
  def paired[A](list: List[A]): Either[String, List[(A, A)]] =
    if (list.size % 2 == 0) {
      list.grouped(2).toList.foldMapM {
        case one :: two :: Nil => Right(List((one, two)))
        case _                 => Left("Should not occur")
      }
    } else {
      Left("the number of elements must be even")
    }
}
