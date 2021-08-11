import cats.implicits.*
import zio.*
import zio.console.{putStrLn, putStrLnErr}
import ZIO.{succeed, fail}
import AppError.*
import Type.*
import Literal.*
import TMapping.*
import LiteralType.*
import ValueType.*
import Expression.*
import TypedExpression.*
import ContextElement.*
import CompilerState.makeExistential
import com.softwaremill.quicklens.*

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
    case (LAtom(_), LTAtom)     => ZIO.unit
    case (LUnit, LTUnit)        => ZIO.unit
    case _ =>
      ZIO.fail(AppError.TypeNotApplicableToLiteral(TLiteral(_type), literal))
  }
}

def assertLiteralChecksAgainst(
    literal: Literal,
    _type: ValueType
): Eff[Unit] = {
  val failure: IO[AppError.TypeNotApplicableToLiteral, Nothing] =
    ZIO.fail(AppError.TypeNotApplicableToLiteral(TValue(_type), literal))
  (literal, _type) match {
    case (LAtom(value), VTAtom(valueType)) =>
      if (valueType == value) ZIO.unit else failure
    case (LChar(value), VTChar(valueType)) =>
      if (valueType == value) ZIO.unit else failure

    case (LString(value), VTString(valueType)) =>
      if (valueType == value) ZIO.unit else failure

    case (LInt(value), VTInt(valueType)) =>
      if (valueType == value) ZIO.unit else failure

    case (LFloat(value), VTFloat(valueType)) =>
      if (valueType == value) ZIO.unit else failure

    case (LBool(value), VTBool(valueType)) =>
      if (valueType == value) ZIO.unit else failure
    case (LUnit, VTUnit) =>
      ZIO.unit
    case _ =>
      ZIO.fail(AppError.TypeNotApplicableToLiteral(TValue(_type), literal))
  }
}

/// Fig 11
def checksAgainst(
    context: Context,
    expr: Expression,
    _type: Type
): Eff[(TypedExpression, Context)] = {
  // TODO add check agains a map/struct ?
  (expr, _type) match {
    case (ELiteral(literal), TValue(valueType)) =>
      assertLiteralChecksAgainst(literal, valueType).as(
        (TELiteral(literal, _type), context)
      )
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
    case (expression, TSum(typeA, typeB)) => {
      ZIO.firstSuccessOf(
        checksAgainst(context, expression, typeA),
        List(checksAgainst(context, expression, typeB))
      )
    }
    // case (map: EMap, _type: TMap) => checksAgainstMap(context, map, _type)
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
    case (expression, it @ TTypeApp(tLambda, types)) => {
      it
        .applyType(context)
        .flatMap {
          case TTypeApp(TExistential(_), _) =>
            for {
              (typed, theta) <- synthesizesTo(context, expr)
              (a, b) <- ZIO.tupled(
                applyContext(typed._type, theta),
                applyContext(_type, theta)
              )
              result <- subtype(theta, a, b)
            } yield (typed, result)
          case it @ TTypeApp(TVariable(_), _) =>
            // rzut na taśmę, przekopiowane z case _ =>
            for {
              (typed, theta) <- synthesizesTo(context, expr)
              (a, b) <- ZIO.tupled(
                applyContext(typed._type, theta),
                applyContext(_type, theta)
              )
              result <- subtype(theta, a, b)
            } yield (typed, result)
          case other => checksAgainst(context, expression, other)
        }
      // it.applyType.flatMap(checksAgainst(context, expression, _))
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
    case _: TValue       => succeed(a)
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
    case it @ TTypeApp(q, args) =>
      for {
        quant <- substitution(context, q, alpha, b)
        args  <- ZIO.foreach(args)(substitution(context, _, alpha, b))
      } yield TTypeApp(quant, args)
    case TExistential(name) => if (name == alpha) succeed(b) else succeed(a)
    case TTuple(valueTypes) =>
      ZIO
        .foreach(valueTypes)(substitution(context, _, alpha, b))
        .map(TTuple(_))
    case TFunction(args, ret) =>
      for {
        argTypes <- ZIO.foreach(args)(substitution(context, _, alpha, b))
        retType  <- substitution(context, ret, alpha, b)
      } yield TFunction(argTypes, retType)
    case TTypeRef(name) =>
      context
        .getTypeDefinition(name)
        .flatMap(substitution(context, _, alpha, b))
    case TMap(mappings) => {
      ZIO
        .foreach(mappings) { it =>
          it match {
            case Required(k, v) =>
              for {
                substitutedK <- substitution(context, k, alpha, b)
                substitutedV <- substitution(context, v, alpha, b)
              } yield Required(substitutedK, substitutedV)
            case Optional(k, v) =>
              for {
                substitutedK <- substitution(context, k, alpha, b)
                substitutedV <- substitution(context, v, alpha, b)
              } yield Optional(substitutedK, substitutedV)
          }

        }
        .map(TMap(_))
    }
    case TSum(x, y) => {
      for {
        substitutedX <- substitution(context, x, alpha, b)
        substitutedY <- substitution(context, y, alpha, b)
      } yield TSum(substitutedX, substitutedY)
    }
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
    case TValue(_)       => succeed(false)
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
    // case it: TTypeApp       => it.applyType.flatMap(occursIn(context, alpha, _))
    case it: TTypeApp =>
      it.applyType(context)
        .flatMap {
          case it: TTypeApp =>
            anyM(it._type :: it.args, occursIn(context, alpha, _))
          case _ => ???
        }
    case TExistential(name) => succeed(alpha == name)
    case TTuple(valueTypes) =>
      anyM(valueTypes, occursIn(context, alpha, _))
    case TTypeRef(name) =>
      context.getTypeDefinition(name).flatMap(occursIn(context, alpha, _))
    case TStruct(fieldTypes) =>
      anyM(fieldTypes.values, occursIn(context, alpha, _))
    case TMap(mappings) =>
      anyM(
        mappings.flatMap(it => List(it.k, it.v)),
        occursIn(context, alpha, _)
      )
    case TSum(x, y) =>
      anyM(List(x, y), occursIn(context, alpha, _))
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
      case (TValue(value), TValue(other)) =>
        assertTrue(value == other, TypesNotEqual(a, b))
          .as(context)
      case (TValue(value), TLiteral(literalType)) =>
        subtype(context, value.literalType, b)
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
              subtype(delta, arg2, arg1)
          }
          a     <- applyContext(ret1, theta)
          b     <- applyContext(ret2, theta)
          delta <- subtype(theta, a, b)
        } yield delta
      case (TMap(kvsA), TMap(kvsB)) => {
        // firstly, the same subtyping logic as in functions apply
        ???
      }
      case (TSum(a1, b1), TSum(a2, b2)) => {
        ???
      }
      case (TSum(x, y), other) => {
        findM(
          List(x, y),
          subtype(context, _, other),
          AppError.CannotSubtype(context, a, b)
        )
        // TODO findM a type in Sum that is a subtype of other
        // anyM(List(a, b), it => subtype(context, ))
      }
      // May not be necessary, if checksAgainst transforms all TTypeRef's into concrete types
      case (TTypeRef(a), other) => ???
      case (other, TTypeRef(a)) => ???
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
      case (it: TMulQuantification, _) =>
        subtype(context, it.desugar, b)
      case (_, it: TMulQuantification) =>
        subtype(context, a, it.desugar)
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
      case (_, TQuantification(name, quantType)) => {
        for {
          theta <- context.add(CVariable(name))
          gamma <- subtype(theta, a, quantType)
          delta <- gamma.drop(CVariable(name))
        } yield delta
      }
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
      // TODO: will need a rework to get HKT to work
      case (TTypeApp(q1, args1), TTypeApp(q2, args2)) => {
        for {
          _ <- assertTrue(
            args1.size == args2.size,
            WrongArity(args2.size, args1.size)
          )
          theta <- ZIO.foldLeft(args1.zip(args2))(context) {
            case (delta, (arg1, arg2)) =>
              subtype(delta, arg1, arg2)
          }
          q1    <- applyContext(q1, theta)
          q2    <- applyContext(q2, theta)
          delta <- subtype(theta, q1, q2)
        } yield delta
      }
      case (it @ TTypeApp(q, args), _) =>
        it.applyType(context)
          .flatMap(subtype(context, _, b))
      case (_, it: TTypeApp) =>
        it.applyType(context)
          .flatMap(subtype(context, a, _))
      // Relic of a fukkup with a non-rebased feature, may need it later
      // case (TSum(subTypes), TSum(types)) =>
      //   ZIO
      //     .foldLeft(subTypes)(context) { case (delta, subType) =>
      //       // TODO remove unsafe head
      //       ZIO.firstSuccessOf(
      //         subtype(context, subType, types.head),
      //         types.tail.map(subtype(context, subType, _))
      //       )
      //     }
      // case (it, TSum(types)) =>
      //   ZIO
      //     .firstSuccessOf(
      //       subtype(context, it, types.head),
      //       types.tail.map(subtype(context, it, _))
      //     )
      // TODO: no subtyping Sum types
      case it => {
        pPrint(it, "#############") *>
          fail(CannotSubtype(context, a, b))
      }
    }
  } yield delta

def literalSynthesizesTo(literal: Literal): Type = {
  literal match {
    case LChar(value)   => TValue(VTChar(value))
    case LString(value) => TValue(VTString(value))
    case LInt(value)    => TValue(VTInt(value))
    case LFloat(value)  => TValue(VTFloat(value))
    case LBool(value)   => TValue(VTBool(value))
    case LAtom(value)   => TValue(VTAtom(value))
    case LNil           => TValue(VTNil)
    case LUnit          => TValue(VTUnit)
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
    case it: TMulQuantification =>
      applicationSynthesizesTo(context, it.desugar, exprs)
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
        (TELiteral(literal, literalSynthesizesTo(literal)), context)
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
        _           <- checkIsWellFormed(context, annType)
        appliedType <- applyContext(annType, context)
        (typedExpression, delta) <- checksAgainst(
          context,
          expression,
          appliedType
        )
      } yield (TEAnnotation(typedExpression, appliedType, appliedType), delta)
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
    case emap: EMap => mapSynthesizesTo(context, emap)
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
        (ifTrueTyped, theta) <- synthesizesTo(delta, ifTrue)
        (ifFalseTyped, psi)  <- synthesizesTo(delta, ifFalse)
        // TODO: think out: maybe this could be avoided by simplifying Sum(A, A) ==> A
        expressionType <- checksAgainst(
          theta,
          ifFalse,
          ifTrueTyped._type
        ).fold(
          _ => TSum(ifTrueTyped._type, ifFalseTyped._type),
          _ => ifTrueTyped._type
        )
      } yield (
        TEIf(typedCondition, ifTrueTyped, ifFalseTyped, expressionType),
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
