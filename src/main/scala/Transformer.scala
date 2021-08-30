import Expression.*
import Literal.*
import cats.parse.{Parser0 as P0, Parser as P, Numbers}
import cats.data.NonEmptyList
import cats.implicits.*
import scala.runtime.stdLibPatches.language.experimental.namedTypeArguments
import cats.Eval

object Transformer {
  def toAstList(exprsList: List[SExp]): Either[String, List[Expression]] =
    exprsList.foldMapM(toAst(_).map(List(_)))

  def toAst(expr: SExp): Either[String, Expression] = {
    expr match {
      case SId(it) =>
        // TODO parse literals like numbers, etc
        parseId(it)
      case SString(it) => ???
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
    // paired(xs).map(_.map { case (l, r) => EMap })
    // TODO done goofed => no map in my language
    ???

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
      case SId(name) :: SSquareList(bindings) :: body :: Nil =>
        for {
          nameExpressionPairs_? <- bindings
            .sliding(2)
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
      case _ => Left("structure of a type annotation:  `(: type expression)`")
    }

  def parseLetBinding(xs: List[SExp]): Either[String, (String, Expression)] =
    xs match {
      case SId(name) :: body :: Nil =>
        toAst(body).map((name, _))
      case _ => Left("structure of a let binding:  `name expression`")
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
    // parsing value types is a big deal
    case SId(value) => ???
    case _          => ???
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
