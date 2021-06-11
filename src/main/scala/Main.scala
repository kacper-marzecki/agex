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

case class SynthResult(typed: List[TypedExpression], context: Context)

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
    case (ELambda(arg, body), TFunction(argType, bodyType)) => {
      val typedVar = CTypedVariable(arg, argType)
      for {
        gamma              <- context.add(typedVar)
        (typedBody, theta) <- checksAgainst(gamma, body, bodyType)
        delta              <- theta.drop(typedVar)
      } yield (TELambda(arg, typedBody, _type), delta)
    }
    //Decl∀I
    case (expression, TQuantification(name, quantType)) => {
      val variable = CVariable(name)
      val gamma    = context.add(variable)
      for {
        (typed, theta) <- checksAgainst(gamma, expression, quantType)
        delta          <- theta.drop(variable)
      } yield (typed, delta)
    }
    case (ETuple(values), TTuple(valueTypes))
        if values.length == valueTypes.length => {
      for {
        // fold right to avoid appending the typedElement to the result list with O(n)
        result <- ZIO.foldRight(values.zip(valueTypes))(
          SynthResult(Nil, context)
        ) { case ((expression, _type), result) =>
          for {
            (elemTyped, gamma) <- checksAgainst(
              result.context,
              expression,
              _type
            )
          } yield SynthResult(elemTyped :: result.typed, gamma)
        }
        resultType = TTuple(result.typed.map(_._type))
      } yield (TETuple(result.typed, resultType), result.context)
    }
    //DeclSub
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
    case TTuple(valueTypes) =>
      TTuple(valueTypes.map(substitution(_, alpha, b)))
    case TFunction(arg, ret) =>
      TFunction(
        substitution(arg, alpha, b),
        substitution(ret, alpha, b)
      )
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
    case TTuple(valueTypes) => valueTypes.exists(occursIn(alpha, _))
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
        assertTrue(isWellFormed(context, a), TypeNotWellFormed(context, a))
          .as(context)
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
      case (TTuple(typesA), TTuple(typesB)) => {
        ZIO.foldLeft(typesA.zip(typesB))(context) { case (delta, (a, b)) =>
          subtype(delta, a, b)
        }
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
          delta  <- subtype(gamma, substitutedQuantType, b)
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
          instantiateR(context, name, a)
        } else {
          fail(CircularInstantiation(context, name, a))
        }
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
    case LUnit      => LTUnit
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
        beta  <- CompilerState.makeExistential
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
    case ETuple(values) => {
      for {
        // fold right to avoid appending the typedElement to the result list with O(n)
        result <-
          ZIO.foldRight(values)(SynthResult(Nil, context)) { (elem, result) =>
            for {
              (elemTyped, gamma) <- synthesizesTo(result.context, elem)
            } yield SynthResult(elemTyped :: result.typed, gamma)
          }
        tupleType = TTuple(result.typed.map(_._type))
      } yield (TETuple(result.typed, tupleType), result.context)
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
    //->E
    case EApplication(fun, arg) => {
      for {
        (funTyped, theta) <- synthesizesTo(context, fun)
        (argTyped, applicationType, delta) <-
          applicationSynthesizesTo(
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
    result     = typedExpression.modify(_._type).setTo(resultType)
    // _ <- prettyPrint(resultContext)
  } yield result
}

object App extends zio.App {
  def run(args: List[String]): zio.URIO[zio.ZEnv, zio.ExitCode] =
    putStrLn("Nothing yet").exitCode
}
