import cats.implicits.*
import zio.*
import zio.console.{putStrLn, putStrLnErr}
import cats.syntax.apply
import ZIO.{succeed, fail}
import AppError.*
import cats.syntax.contravariantSemigroupal
import Type.*
import Literal.*
import LiteralType.*
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
    case (LUnit, LTUnit)        => ZIO.unit
    case _ => ZIO.fail(AppError.TypeNotApplicableToLiteral(_type, literal))
  }
}

def literalSynthesizesTo(literal: Literal): LiteralType = {
  literal match {
    case LChar(_)   => LTChar
    case LString(_) => LTString
    case LInt(_)    => LTInt
    case LFloat(_)  => LTFloat
    case LBool(_)   => LTBool
    case LUnit      => LTUnit
  }
}

/// Fig 11
def checksAgainst(
    context: Context,
    expr: Expression,
    _type: Type
): Eff[(TypedExpression, Context)] = {
  (expr, _type) match {
    //1I
    case (ELiteral(literal), TLiteral(_type)) => {
      assertLiteralChecksAgainst(literal, _type).as(
        (TELiteral(literal, TLiteral(_type)), context)
      )
    }
    //->I
    case (
          ELambda(arg, body),
          TFunction(argType, bodyType)
        ) => {
      val typedVar = CTypedVariable(arg, argType)
      for {
        gamma <- context.add(typedVar)
        (typedBody, theta) <- checksAgainst(gamma, body, bodyType)
        delta <- theta.drop(typedVar)
      } yield (TELambda(arg, typedBody, _type), delta)
    }
    case (expression, TQuantification(name, quantType)) => {
      val variable = CVariable(name)
      val gamma = context.add(variable)
      for {
        (typed, theta) <- checksAgainst(gamma, expression, quantType)
        delta <- theta.drop(variable)
      } yield (typed, delta)
    }
    //xI
    case (ETuple(one, two), TProduct(oneType, twoType)) => {
      for {
        (typedOne, gamma) <- checksAgainst(context, one, oneType)
        (typedTwo, theta) <- checksAgainst(gamma, two, twoType)
      } yield (TETuple(typedOne, typedTwo, _type), theta)
    }
    //Sub
    case _ => {
      for {
        (typed, theta) <- synthesizesTo(context, expr)
        result <- subtype(
          theta,
          applyContext(typed._type, theta),
          applyContext(_type, theta)
        )
      } yield (typed, result)
    }
  }
}

//Fig. 11
// returns (typed expression of the argument, return type of the function, context)
def applicationSynthesizesTo(
    context: Context,
    _type: Type,
    expr: Expression
): Eff[(TypedExpression, Type, Context)] = {
  _type match {
    //alphaApp
    case TExistential(name) => {
      for {
        alpha1 <- CompilerState.makeExistential
        alpha2 <- CompilerState.makeExistential
        gamma <- context.insertInPlace(
          CExistential(name),
          List(
            CExistential(alpha2),
            CExistential(alpha1),
            CSolved(
              name,
              TFunction(
                TExistential(alpha1),
                TExistential(alpha2)
              )
            )
          )
        )
        (typedExpr, delta) <- checksAgainst(gamma, expr, TExistential(alpha1))
      } yield (typedExpr, TExistential(alpha2), delta)
    }
    //ForallApp
    case TQuantification(name, quantType) => {
      for {
        alpha <- CompilerState.makeExistential
        gamma = context.add(CExistential(alpha))
        substitutedType = substitution(
          quantType,
          name,
          TExistential(alpha)
        )
        result <- applicationSynthesizesTo(gamma, substitutedType, expr)
      } yield result
    }
    //App
    case TFunction(arg, ret) =>
      for {
        (typedExpr, delta) <- checksAgainst(context, expr, arg)
      } yield (typedExpr, ret, delta)
    case _ => fail(CannotApplyType(_type))
  }
}

/// a is replaced with b in all occurrences in A
def substitution(a: Type, alpha: String, b: Type): Type = {
  a match {
    case _: TLiteral     => a
    case TVariable(name) => if (name == alpha) b else a
    case TQuantification(name, quantType) => {
      if (name == alpha) {
        TQuantification(name, b)
      } else {
        TQuantification(name, substitution(quantType, alpha, b))
      }
    }
    case TExistential(name) => if (name == alpha) b else a
    case TProduct(one, two) =>
      TProduct(
        substitution(one, alpha, b),
        substitution(two, alpha, b)
      )
    case TFunction(arg, ret) =>
      TFunction(
        substitution(arg, alpha, b),
        substitution(ret, alpha, b)
      )
  }
}

def assertTrue[E](cond: Boolean, ifFail: => E): IO[E, Unit] =
  if (cond) {
    ZIO.unit
  } else {
    ZIO.fail(ifFail)
  }

/// Fig 7
def isWellFormed(context: Context, _type: Type): Boolean = {
  _type match {
    case _: TLiteral     => true
    case TVariable(name) => context.hasVariable(name)
    case TFunction(arg, ret) =>
      isWellFormed(context, arg) && isWellFormed(context, ret)
    case TQuantification(alpha, a) =>
      isWellFormed(context.add(CVariable(alpha)), a)
    case TExistential(name) =>
      context.hasExistential(name) || context.getSolved(name).isDefined
    case TProduct(one, two) =>
      isWellFormed(context, one) && isWellFormed(context, two)
  }
}

// Fig 9: α^ !∈ FV(B)
def occursIn(alpha: String, a: Type): Boolean = {
  a match {
    case TLiteral(_)     => false
    case TVariable(name) => alpha == name
    case TFunction(arg, ret) =>
      occursIn(alpha, arg) || occursIn(alpha, ret)
    case TQuantification(beta, t) => {
      if (alpha == beta) {
        return true;
      } else {
        return occursIn(alpha, t);
      }
    }
    case TExistential(name) => alpha == name
    case TProduct(one, two) => occursIn(alpha, one) || occursIn(alpha, two)
  }
}

def subtype(context: Context, a: Type, b: Type): Eff[Context] =
  for {
    _ <- assertTrue(isWellFormed(context, a), TypeNotWellFormed(context, a))
    _ <- assertTrue(isWellFormed(context, b), TypeNotWellFormed(context, b))
    delta <- (a, b) match {
      //<:Unit
      case (TLiteral(literalA), TLiteral(literalB)) => {
        assertTrue(literalA == literalB, TypesNotEqual(a, b))
          .as(context)
      }
      //<:Var
      case (TVariable(nameA), TVariable(nameB)) => {
        assertTrue(isWellFormed(context, a), TypeNotWellFormed(context, a)) *>
          assertTrue(nameA == nameB, TypeNamesNotEqual(nameA, nameB))
            .as(context)
      }
      //<:Exvar
      case (TExistential(name1), TExistential(name2)) if name1 == name2 => {
        assertTrue(isWellFormed(context, a), TypeNotWellFormed(context, a)).as(
          context
        )
      }
      //<:->
      case (TFunction(arg1, ret1), TFunction(arg2, ret2)) => {
        for {
          theta <- subtype(context, arg1, arg2)
          delta <- subtype(
            theta,
            applyContext(ret1, theta),
            applyContext(ret2, theta)
          )
        } yield delta
      }
      case (TProduct(one1, two1), TProduct(one2, two2)) => {
        subtype(context, one1, one2).flatMap(subtype(_, two1, two2))
      }
      //<:forallL
      case (TQuantification(name, quantType), _) => {
        for {
          alpha <- CompilerState.makeExistential
          gamma = context
            .add(CMarker(alpha))
            .add(CExistential(alpha))
          substitutedQuantType = substitution(
            quantType,
            name,
            TExistential(alpha)
          )
          delta <- subtype(gamma, substitutedQuantType, b)
          result <- delta.drop(CMarker(alpha))
        } yield result
      }
      //<:forallR
      case (_, TQuantification(name, quantType)) => {
        val theta = context.add(CVariable(name))
        subtype(theta, a, quantType)
          .flatMap(_.drop(CVariable(name)))
      }
      //<:InstatiateL
      case (TExistential(name), _) => {
        if (!occursIn(name, b)) {
          instantiateL(context, name, b)
        } else {
          fail(CircularInstantiation(context, name, b))
        }
      }
      //<:InstantiateR
      case (_, TExistential(name)) => {
        if (!occursIn(name, a)) {
          instantiateR(context, a, name)
        } else {
          fail(CircularInstantiation(context, name, a))
        }
      }
      case _ => fail(CannotSubtype(context, a, b))
    }
  } yield delta

// Fig 10
def instantiateL(context: Context, alpha: String, b: Type): Eff[Context] = {
  for {
    split <- context.splitAt(CExistential(alpha))
    (left, right) = split
    //InstLSolve
    result <-
      if (b.isMonotype && isWellFormed(left, b)) {
        context.insertInPlace(
          CExistential(alpha),
          List(CSolved(alpha, b))
        )
      } else {
        b match {
          //InstLArr
          case TFunction(arg, ret) => {
            for {
              alpha1 <- CompilerState.makeExistential
              alpha2 <- CompilerState.makeExistential
              gamma <- context.insertInPlace(
                CExistential(alpha),
                List(
                  CExistential(alpha2),
                  CExistential(alpha1),
                  CSolved(
                    alpha,
                    TFunction(
                      TExistential(alpha1),
                      TExistential(alpha2)
                    )
                  )
                )
              )
              theta <- instantiateR(gamma, arg, alpha1)
              delta <- instantiateL(theta, alpha2, applyContext(ret, theta))
            } yield delta
          }
          //InstAIIR
          case TQuantification(beta, b) => {
            for {
              gamma <- instantiateL(
                context.add(CVariable(beta)),
                alpha,
                b
              )
              delta <- gamma.drop(CVariable(beta))
            } yield delta
          }
          //InstLReach
          case TExistential(beta) => {
            context.insertInPlace(
              CExistential(beta),
              List(CSolved(beta, TExistential(alpha)))
            )
          }
          case _ => fail(CannotInstantiateL(context, alpha, b))
        }
      }
  } yield result
}

/// Fig 10
def instantiateR(context: Context, a: Type, alpha: String): Eff[Context] =
  for {
    split <- context.splitAt(CExistential(alpha))
    (left, right) = split
    result <-
      if (a.isMonotype && isWellFormed(left, a)) {
        //InstRSolve
        context.insertInPlace(
          CExistential(alpha),
          List(CSolved(alpha, a))
        )
      } else {
        a match {
          //InstRArr
          case TFunction(arg, ret) => {
            for {
              alpha1 <- CompilerState.makeExistential
              alpha2 <- CompilerState.makeExistential
              gamma <- context.insertInPlace(
                CExistential(alpha),
                List(
                  CExistential(alpha2),
                  CExistential(alpha1),
                  CSolved(
                    alpha,
                    TFunction(
                      TExistential(alpha1),
                      TExistential(alpha2)
                    )
                  )
                )
              )
              theta <- instantiateL(gamma, alpha1, arg)
              delta <- instantiateR(theta, applyContext(ret, theta), alpha2)
            } yield delta
          }
          //InstRAllL
          case TQuantification(beta, b) => {
            for {
              beta1 <- CompilerState.makeExistential
              gamma = context
                .add(CMarker(beta1))
                .add(CExistential(beta1))
              theta <- instantiateR(
                gamma,
                substitution(b, beta, TExistential(beta1)),
                alpha
              )
              delta <- theta.drop(CMarker(beta1))
            } yield delta
          }
          case TProduct(one, two) => {
            for {
              alpha1 <- CompilerState.makeExistential
              beta1 <- CompilerState.makeExistential
              gamma <- context.insertInPlace(
                CExistential(alpha),
                List(
                  CExistential(beta1),
                  CExistential(alpha1),
                  CSolved(
                    alpha,
                    TProduct(
                      TExistential(alpha1),
                      TExistential(beta1)
                    )
                  )
                )
              )
              theta <- instantiateL(gamma, alpha1, one)
              delta <- instantiateR(theta, applyContext(two, theta), beta1)
            } yield delta
          }
          //InstRReach
          case TExistential(beta) => {
            context.insertInPlace(
              CExistential(beta),
              List(CSolved(beta, TExistential(alpha)))
            )
          }
          case _ => fail(CannotInstantiateR(context, alpha, a))
        }
      }
  } yield result

/// Fig 8
def applyContext(_type: Type, context: Context): Type = {
  _type match {
    case TLiteral(_)  => _type
    case TVariable(_) => _type
    case TExistential(name) => {
      context.getSolved(name).fold(_type)(applyContext(_, context))
    }
    case TFunction(argType, returnType) =>
      TFunction(
        applyContext(argType, context),
        applyContext(returnType, context)
      )
    case TQuantification(name, quantType) => {
      TQuantification(name, applyContext(quantType, context))
    }
    case TProduct(oneType, twoType) =>
      TProduct(
        applyContext(oneType, context),
        applyContext(twoType, context)
      )
  }
}

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
      if (isWellFormed(context, annType)) {
        for {
          (typedExpression, delta) <- checksAgainst(
            context,
            expression,
            annType
          )
        } yield (TEAnnotation(typedExpression, annType, annType), delta)
      } else {
        fail(TypeNotWellFormed(context, annType))
      }
    }
    //->I=>
    case ELambda(arg, ret) => {
      for {
        alpha <- CompilerState.makeExistential
        beta <- CompilerState.makeExistential
        gamma <-
          context
            .add(CExistential(alpha))
            .add(CExistential(beta))
            .add(CTypedVariable(arg, TExistential(alpha)))
        (typedRet, theta) <- checksAgainst(gamma, ret, TExistential(beta))
        delta <- theta.drop(CTypedVariable(arg, TExistential(alpha)))
        functionType = TFunction(TExistential(alpha), TExistential(beta))
      } yield (
        TELambda(arg, typedRet, functionType),
        delta
      )
    }
    case ETuple(one, two) => {
      for {
        (oneTyped, gamma) <- synthesizesTo(context, one)
        (twoTyped, delta) <- synthesizesTo(gamma, two)
        tupleType = TProduct(oneTyped._type, twoTyped._type)
      } yield (TETuple(oneTyped, twoTyped, tupleType), delta)
    }
    case ELet(name, expr, body) => {
      for {
        (exprTyped, gamma) <- synthesizesTo(context, expr)
        exprVariable = CTypedVariable(name, exprTyped._type)
        theta <- gamma.add(exprVariable)
        (bodyTyped, phi) <- synthesizesTo(theta, body)
        delta <- phi.insertInPlace(exprVariable, List())
      } yield (TELet(name, exprTyped, bodyTyped, bodyTyped._type), delta)
    }
    //->E
    case EApplication(fun, arg) => {
      for {
        (funTyped, theta) <- synthesizesTo(context, fun)
        (argTyped, applicationType, delta) <- applicationSynthesizesTo(
          theta,
          applyContext(funTyped._type, theta),
          arg
        )
      } yield (TEApplication(funTyped, argTyped, applicationType), delta)
    }
  }
}

def synth(
    expr: Expression,
    context: Context = Context()
): Eff[TypedExpression] = {
  for {
    (typedExpression, resultContext) <- synthesizesTo(context, expr)
    resultType = applyContext(typedExpression._type, resultContext)
    result = typedExpression.modify(_._type).setTo(resultType)
    // _ <- prettyPrint(resultContext)
  } yield result
}

object App extends zio.App {
  def run(args: List[String]): zio.URIO[zio.ZEnv, zio.ExitCode] =
    putStrLn("Nothing yet").exitCode
}
