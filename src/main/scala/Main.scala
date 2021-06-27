import cats.implicits.*
import zio.*
import zio.console.{putStrLn, putStrLnErr}
import ZIO.{succeed, fail}
import AppError.*
import Type.*
import Literal.*
import LiteralType.*
import Expression.*
import TypedExpression.*
import ContextElement.*
import CompilerState.makeExistential
import com.softwaremill.quicklens.*
import javax.imageio.plugins.tiff.TIFFField

def assertLiteralChecksAgainst(
    literal: Literal,
    _type: LiteralType
): Eff[Unit] = {
  (literal, _type) match {
    case (LChar(_), LTChar)     => ZIO.unit
    case (LString(_), LTString) => ZIO.unit
    case (LInt(_), LTInt)       => ZIO.unit
    case (LFloat(_), LTFloat)   => ZIO.unit
    case (LBool(_), LTBool)     => ZIO.unit
    case (LUnit, LTUnit)        => ZIO.unit
    case _ => ZIO.fail(AppError.TypeNotApplicableToLiteral(_type, literal))
  }
}

/// Fig 11
def checksAgainst(
    context: Context,
    expr: Expression,
    _type: Type
): Eff[(TypedExpression, Context)] = {
  (expr, _type) match {
    //Decl1I
    case (ELiteral(literal), TLiteral(_type)) => {
      assertLiteralChecksAgainst(literal, _type).as(
        (TELiteral(literal, TLiteral(_type)), context)
      )
    }
    //Decl→I
    case (EFunction(args, body), TFunction(argTypes, bodyType)) => {
      val typedVars = args.zip(argTypes).map(CTypedVariable.apply.tupled)
      for {
        _ <- assertTrue(
          args.size == argTypes.size,
          WrongArity(argTypes.size, args.size)
        )
        gamma              <- context.addAll(typedVars)
        (typedBody, theta) <- checksAgainst(gamma, body, bodyType)
        delta              <- theta.drop(typedVars)
      } yield (TEFunction(args, typedBody, _type), delta)
    }
    //Decl∀I
    case (expression, TQuantification(name, quantType)) => {
      val variable = CVariable(name)
      for {
        gamma          <- context.add(variable)
        (typed, theta) <- checksAgainst(gamma, expression, quantType)
        delta          <- theta.drop(variable)
      } yield (typed, delta)
    }
    case (expression, it: TMulQuantification) =>
      checksAgainst(context, expression, it.desugar).map {
        // restoring the original TMulQuantification type
        case (typed, delta) => (typed.modify(_._type).setTo(it), delta)
      }
    case (ETuple(values), TTuple(valueTypes))
        if values.length == valueTypes.length => {
      for {
        // fold right to avoid appending the typedElement to the result list with O(n)
        result <- ZIO.foldRight(values.zip(valueTypes))(
          TEAggregation(Nil, context)
        ) { case ((expression, _type), result) =>
          for {
            (elemTyped, gamma) <- checksAgainst(
              result.context,
              expression,
              _type
            )
          } yield TEAggregation(elemTyped :: result.typed, gamma)
        }
        resultType = TTuple(result.typed.map(_._type))
      } yield (TETuple(result.typed, resultType), result.context)
    }
    case (_, TTypeRef(name)) =>
      context.getTypeDefinition(name).flatMap(checksAgainst(context, expr, _))
    case (EIf(condition, ifTrue, ifFalse), _type) => {
      for {
        _ <- prettyPrint("EIf", "ChecksAgainst")
        (conditionTyped, gamma) <- checksAgainst(
          context,
          ifTrue,
          TLiteral(LTBool)
        )
        (ifTrueTyped, theta)  <- checksAgainst(gamma, ifTrue, _type)
        (ifFalseTyped, delta) <- checksAgainst(theta, ifFalse, _type)
      } yield (TEIf(conditionTyped, ifTrueTyped, ifFalseTyped, _type), delta)
    }
    //DeclSub
    case _ => {
      for {
        (typed, theta) <- synthesizesTo(context, expr)
        (a, b) <- ZIO.tupled(
          applyContext(typed._type, theta),
          applyContext(_type, theta)
        )
        result <- subtype(theta, a, b)
      } yield (typed, result)
    }
  }
}

/// a is replaced with b in all occurrences in A
def substitution(
    context: Context,
    a: Type,
    alpha: String,
    b: Type
): IO[AppError, Type] = {
  a match {
    case _: TLiteral     => succeed(a)
    case TVariable(name) => if (name == alpha) succeed(b) else succeed(a)
    case TQuantification(name, quantType) => {
      if (name == alpha) {
        succeed(TQuantification(name, b))
      } else {
        substitution(context, quantType, alpha, b).map(TQuantification(name, _))
      }
    }
    // 1:1 port of TQuantification
    case TMulQuantification(names, quantType) => {
      if (names.contains(alpha)) {
        succeed(TMulQuantification(names, b))
      } else {
        substitution(context, quantType, alpha, b).map(
          TMulQuantification(names, _)
        )
      }
    }
    case TExistential(name) => if (name == alpha) succeed(b) else succeed(a)
    case TTuple(valueTypes) =>
      ZIO.foreach(valueTypes)(substitution(context, _, alpha, b)).map(TTuple(_))
    case TFunction(args, ret) =>
      for {
        argTypes <- ZIO.foreach(args)(substitution(context, _, alpha, b))
        retType  <- substitution(context, ret, alpha, b)
      } yield TFunction(argTypes, retType)
    case TTypeRef(name) =>
      context
        .getTypeDefinition(name)
        .flatMap(substitution(context, _, alpha, b))
    case TStruct(fieldTypes) =>
      ZIO
        .foreach(fieldTypes) { case (k, v) =>
          substitution(context, v, alpha, b).map((k, _))
        }
        .map(TStruct.apply)
  }
}

// Fig 9: α^ !∈ FV(B)
def occursIn(
    context: Context,
    alpha: String,
    a: Type
): IO[AppError, Boolean] = {
  a match {
    case TLiteral(_)     => succeed(false)
    case TVariable(name) => succeed(alpha == name)
    case TFunction(args, ret) =>
      anyM(ret :: args, occursIn(context, alpha, _))
    case TQuantification(beta, t) => {
      if (alpha == beta) {
        return succeed(true);
      } else {
        return occursIn(context, alpha, t);
      }
    }
    case it: TMulQuantification => occursIn(context, alpha, it.desugar)
    case TExistential(name)     => succeed(alpha == name)
    case TTuple(valueTypes) =>
      anyM(valueTypes, occursIn(context, alpha, _))
    case TTypeRef(name) =>
      context.getTypeDefinition(name).flatMap(occursIn(context, alpha, _))
    case TStruct(fieldTypes) =>
      anyM(fieldTypes.values, occursIn(context, alpha, _))
  }
}

// Ψ ⊢ A ≤ B Under context Ψ, type A is a subtype of B
def subtype(context: Context, a: Type, b: Type): Eff[Context] =
  for {
    _ <- checkIsWellFormed(context, a)
    _ <- checkIsWellFormed(context, b)
    delta <- (a, b) match {
      //<:Unit
      case (TLiteral(literalA), TLiteral(literalB)) => {
        assertTrue(literalA == literalB, TypesNotEqual(a, b))
          .as(context)
      }
      //<:Var
      case (TVariable(nameA), TVariable(nameB)) => {
        checkIsWellFormed(context, a) *>
          assertTrue(nameA == nameB, TypeNamesNotEqual(nameA, nameB))
            .as(context)
      }
      //<:Exvar
      case (TExistential(name1), TExistential(name2)) if name1 == name2 => {
        checkIsWellFormed(context, a).as(context)
      }
      //<:->
      case (TFunction(args1, ret1), TFunction(args2, ret2)) =>
        for {
          _ <- assertTrue(
            args1.size == args2.size,
            WrongArity(args2.size, args1.size)
          )
          theta <- ZIO.foldLeft(args1.zip(args2))(context) {
            case (delta, (arg1, arg2)) =>
              subtype(delta, arg1, arg2)
          }
          a     <- applyContext(ret1, theta)
          b     <- applyContext(ret2, theta)
          delta <- subtype(theta, a, b)
        } yield delta
      case (TTuple(typesA), TTuple(typesB)) => {
        if (typesA.size != typesB.size) {
          fail(TupleSizesDontMatch(TTuple(typesA), TTuple(typesB)))
        } else {
          ZIO.foldLeft(typesA.zip(typesB))(context) { case (delta, (a, b)) =>
            subtype(delta, a, b)
          }
        }
      }
      case (TStruct(fieldsA), TStruct(fieldsB)) => {
        val extractedKeys = extractKeys(fieldsA, fieldsB.keys)
        if (!extractedKeys.notFound.isEmpty) {
          fail(MissingFields(extractedKeys.notFound.toList))
        } else {
          val commonFields = fieldsB.keys.toList.mapFilter(it =>
            (extractedKeys.included.get(it), fieldsB.get(it)).bisequence
          )
          ZIO.foldLeft(commonFields)(context) { case (delta, (a, b)) =>
            subtype(delta, a, b)
          }
        }
      }
      //<:∀L
      case (TQuantification(name, quantType), _) => {
        for {
          alpha <- CompilerState.makeExistential
          gamma <- context
            .addAll(CMarker(alpha), CExistential(alpha))
          substitutedQuantType <- substitution(
            context,
            quantType,
            name,
            TExistential(alpha)
          )
          delta  <- subtype(gamma, substitutedQuantType, b)
          result <- delta.drop(CMarker(alpha))
        } yield result
      }
      //<:∀R
      case (_, TQuantification(name, quantType)) =>
        for {
          theta <- context.add(CVariable(name))
          gamma <- subtype(theta, a, quantType)
          delta <- gamma.drop(CVariable(name))
        } yield delta
      //<:InstantiateL
      case (TExistential(name), _) => {
        assertNotM(
          occursIn(context, name, b),
          CircularInstantiation(context, name, b)
        ) *>
          instantiateL(context, name, b)
      }
      //<:InstantiateR
      case (_, TExistential(name)) => {
        assertNotM(
          occursIn(context, name, a),
          CircularInstantiation(context, name, a)
        ) *>
          instantiateR(context, name, a)
      }
      case _ => fail(CannotSubtype(context, a, b))
    }
  } yield delta

def literalSynthesizesTo(literal: Literal): LiteralType = {
  literal match {
    case LChar(_)   => LTChar
    case LString(_) => LTString
    case LInt(_)    => LTInt
    case LFloat(_)  => LTFloat
    case LBool(_)   => LTBool
    case LAtom(_)   => LTAtom
    case LNil       => LTNil
    case LUnit      => LTUnit
  }
}

//Fig. 11
// returns (typed argument expressions, return type of the function, context)
def applicationSynthesizesTo(
    context: Context,
    functionType: Type,
    exprs: List[Expression]
): Eff[(List[TypedExpression], Type, Context)] = {
  functionType match {
    case TExistential(name) => {
      for {
        retAlpha  <- CompilerState.makeExistential
        argAlphas <- ZIO.foreach(exprs)(_ => CompilerState.makeExistential)
        gamma <- context.insertInPlace(
          CExistential(name),
          CExistential(retAlpha) ::
            // reverse to place the first argument α^ to the right in the context
            argAlphas.reverse.map(CExistential(_)) :::
            List(
              CSolved(
                name,
                TFunction(
                  argAlphas.map(TExistential(_)),
                  TExistential(retAlpha)
                )
              )
            )
        )
        result <- ZIO.foldLeft(argAlphas.zip(exprs))(
          TEAggregation(Nil, gamma)
        ) { case (result, (alpha1, expr)) =>
          for {
            (elemTyped, gamma) <- checksAgainst(
              result.context,
              expr,
              TExistential(alpha1)
            )
          } yield TEAggregation(elemTyped :: result.typed, gamma)
        }
      } yield (result.typed, TExistential(retAlpha), result.context)
    }
    //∀App
    case TQuantification(name, quantType) => {
      for {
        alpha <- CompilerState.makeExistential
        gamma <- context.add(CExistential(alpha))
        substitutedType <- substitution(
          context,
          quantType,
          name,
          TExistential(alpha)
        )
        result <- applicationSynthesizesTo(gamma, substitutedType, exprs)
      } yield result
    }
    //→App
    case TFunction(argTypes, ret) =>
      for {
        _ <- assertTrue(
          argTypes.size == exprs.size,
          WrongArity(argTypes.size, exprs.size)
        )
        res <- ZIO.foldLeft(exprs.zip(argTypes))(TEAggregation(Nil, context)) {
          case (acc, (arg, argType)) =>
            for {
              (typed, delta) <- checksAgainst(acc.context, arg, argType)
            } yield (TEAggregation(acc.typed :+ typed, delta))
        }
      } yield (res.typed, ret, res.context)
    case _ => fail(CannotApplyType(functionType))
  }
}

/** Aggregation of typedExpressions, under a Context
  */
case class TEAggregation(typed: List[TypedExpression], context: Context)
def synthesizesTo(
    context: Context,
    exprs: Iterable[Expression]
): Eff[TEAggregation] =
  // fold right to avoid appending the typedElement to the result list with O(n)
  ZIO.foldRight(exprs)(TEAggregation(Nil, context)) { (elem, result) =>
    for {
      (elemTyped, gamma) <- synthesizesTo(result.context, elem)
    } yield TEAggregation(elemTyped :: result.typed, gamma)
  }

// Figure 11. Algorithmic typing
def synthesizesTo(
    context: Context,
    expr: Expression
): Eff[(TypedExpression, Context)] = {
  expr match {
    //1I=>
    case ELiteral(literal) =>
      succeed(
        (TELiteral(literal, TLiteral(literalSynthesizesTo(literal))), context)
      )
    //Var
    case EVariable(name) => {
      context
        .getAnnotation(name)
        .fold(ZIO.fail(AnnotationNotFound(context, name)))(annotation =>
          succeed((TEVariable(name, annotation), context))
        )
    }
    //Anno
    case EAnnotation(expression, annType) => {
      for {
        _ <- checkIsWellFormed(context, annType)
        (typedExpression, delta) <- checksAgainst(
          context,
          expression,
          annType
        )
      } yield (TEAnnotation(typedExpression, annType, annType), delta)
    }
    case ETypeAlias(newName, targetType, expr) => {
      for {
        theta              <- context.add(CTypeDefinition(newName, targetType))
        (typedExpr, gamma) <- synthesizesTo(theta, expr)
        // potential pain point: may need gamma.insertInPlace(CTypeDefinition(newName, targetType), List())
        delta <- gamma.drop(CTypeDefinition(newName, targetType))
      } yield (
        TETypeAlias(newName, targetType, typedExpr, typedExpr._type),
        delta
      )
    }
    //→I⇒
    case EFunction(args, ret) => {
      for {
        (sigmas, sigmaVariables) <- ZIO
          .foreach(args)(arg =>
            CompilerState.makeExistential
              .map(sigma => (sigma, CTypedVariable(arg, TExistential(sigma))))
          )
          .map(it => split(it))
        tau <- CompilerState.makeExistential
        gamma <- context.addAll(
          sigmas.map(CExistential(_)) :::
            List(CExistential(tau)) :::
            sigmaVariables
        )
        (typedRet, theta) <- checksAgainst(gamma, ret, TExistential(tau))
        delta             <- theta.drop(sigmaVariables)
        functionType = TFunction(sigmas.map(TExistential(_)), TExistential(tau))
      } yield (
        TEFunction(args, typedRet, functionType),
        delta
      )
    }
    case ETuple(values) => {
      for {
        result <- synthesizesTo(context, values)
        tupleType = TTuple(result.typed.map(_._type))
      } yield (TETuple(result.typed, tupleType), result.context)
    }
    case EStruct(fields) => {
      for {
        result <- synthesizesTo(context, fields.values)
        typedFields = fields.keys.zip(result.typed).toMap
        fieldTypes = typedFields.map { case (fieldName, typedField) =>
          (fieldName, typedField._type)
        }.toMap
      } yield (TEStruct(typedFields, TStruct(fieldTypes)), result.context)
    }
    case ELet(name, expr, body) => {
      for {
        (exprTyped, gamma) <- synthesizesTo(context, expr)
        exprVariable = CTypedVariable(name, exprTyped._type)
        theta            <- gamma.add(exprVariable)
        (bodyTyped, phi) <- synthesizesTo(theta, body)
        delta            <- phi.insertInPlace(exprVariable, List())
      } yield (TELet(name, exprTyped, bodyTyped, bodyTyped._type), delta)
    }
    //→E
    case EFunctionApplication(fun, args) => {
      for {
        (funTyped, theta) <- synthesizesTo(context, fun)
        appliedFunType    <- applyContext(funTyped._type, theta)
        (argsTyped, applicationType, delta) <-
          applicationSynthesizesTo(
            theta,
            appliedFunType,
            args
          )
      } yield (
        TEFunctionApplication(funTyped, argsTyped, applicationType),
        delta
      )
    }
    case EIf(condition, ifTrue, ifFalse) => {
      for {
        (typedCondition, delta) <- checksAgainst(
          context,
          condition,
          TLiteral(LTBool)
        )
        // TODO: find a common supertype ?
        (ifTrueTyped, theta) <- synthesizesTo(delta, ifTrue)
        (ifFalseTyped, delta) <- checksAgainst(
          theta,
          ifFalse,
          ifTrueTyped._type
        )
      } yield (
        TEIf(typedCondition, ifTrueTyped, ifFalseTyped, ifTrueTyped._type),
        delta
      )
    }
  }
}

def synth(
    expr: Expression,
    context: Context = Context()
): Eff[(Context, TypedExpression)] = {
  for {
    (typedExpression, resultContext) <- synthesizesTo(context, expr)
    resultType <- applyContext(typedExpression._type, resultContext)
    result = typedExpression.modify(_._type).setTo(resultType)
  } yield (resultContext, result)
}

object App extends zio.App {
  def run(args: List[String]): zio.URIO[zio.ZEnv, zio.ExitCode] =
    putStrLn("Nothing yet").exitCode
}
