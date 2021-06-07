import cats.implicits.*
import zio.*
import zio.console.{putStrLn, putStrLnErr}
import cats.syntax.apply
import ZIO.{succeed, fail}
import AppError.*
import cats.syntax.contravariantSemigroupal

def assertLiteralChecksAgainst(
    literal: Literal,
    _type: LiteralType
): Eff[Unit] = {
  (literal, _type) match {
    case (Literal.LChar(_), LiteralType.LTChar)     => ZIO.unit
    case (Literal.LString(_), LiteralType.LTString) => ZIO.unit
    case (Literal.LInt(_), LiteralType.LTInt)       => ZIO.unit
    case (Literal.LFloat(_), LiteralType.LTFloat)   => ZIO.unit
    case (Literal.LBool(_), LiteralType.LTBool)     => ZIO.unit
    case (Literal.LUnit, LiteralType.LTUnit)        => ZIO.unit
    case _ => ZIO.fail(AppError.TypeNotApplicableToLiteral(_type, literal))
  }
}

// TODO rename /w overload
def literalSynthesizesTo(literal: Literal): LiteralType = {
  literal match {
    case Literal.LChar(_)   => LiteralType.LTChar
    case Literal.LString(_) => LiteralType.LTString
    case Literal.LInt(_)    => LiteralType.LTInt
    case Literal.LFloat(_)  => LiteralType.LTFloat
    case Literal.LBool(_)   => LiteralType.LTBool
    case Literal.LUnit      => LiteralType.LTUnit
  }
}

/// Figure 11.
def checksAgainst(
    context: Context,
    expr: Expression,
    _type: Type
): Eff[Context] = {
  (expr, _type) match {
    //1I
    case (Expression.ELiteral(literal), Type.TLiteral(_type)) => {
      assertLiteralChecksAgainst(literal, _type).as(context)
    }
    //->I
    case (
          Expression.EAbstraction(arg, body),
          Type.TFunction(argType, bodyType)
        ) => {
      val typedVar = ContextElement.TypedVariable(arg, argType)
      val gamma = context.add(typedVar)
      checksAgainst(gamma, body, bodyType)
        .flatMap(_.drop(typedVar))
    }
    case (expression, Type.TQuantification(name, quantType)) => {
      val variable = ContextElement.Variable(name)
      val gamma = context.add(variable)
      checksAgainst(gamma, expression, quantType).flatMap(_.drop(variable))
    }
    //xI
    case (Expression.ETuple(one, two), Type.TProduct(oneType, twoType)) => {
      checksAgainst(context, one, oneType).flatMap(
        checksAgainst(_, two, twoType)
      )
    }
    //Sub
    case _ => {
      synthesizesTo(context, expr)
        .flatMap { case (a, theta) =>
          subtype(
            theta,
            applyContext(a, theta),
            applyContext(_type, theta)
          )
        }
    }
  }
}

def synthesizesTo(context: Context, expr: Expression): Eff[(Type, Context)] =
  expr match {
    //1I=>
    case Expression.ELiteral(literal) =>
      succeed((Type.TLiteral(literalSynthesizesTo(literal)), context))
    //Var
    case Expression.EVariable(name) => {
      context
        .getAnnotation(name)
        .fold(ZIO.fail(AnnotationNotFound(context, name)))(annotation =>
          succeed((annotation, context))
        )
    }
    //Anno
    case Expression.EAnnotation(expression, annType) => {
      if (isWellFormed(context, annType)) {
        val delta = checksAgainst(context, expression, annType)
        delta.map(it => (annType, it))
      } else {
        fail(TypeNotWellFormed(context, annType))
      }
    }
    //->I=>
    case Expression.EAbstraction(arg, ret) => {
      for {
        alpha <- MutableState.makeExistential
        beta <- MutableState.makeExistential
        gamma = context
          .add(ContextElement.Existential(alpha))
          .add(ContextElement.Existential(beta))
          .add(ContextElement.TypedVariable(arg, Type.TExistential(alpha)))
        delta <- checksAgainst(gamma, ret, Type.TExistential(beta))
          .flatMap(
            _.drop(ContextElement.TypedVariable(arg, Type.TExistential(alpha)))
          )
      } yield (
        Type.TFunction(Type.TExistential(alpha), Type.TExistential(beta)),
        delta
      )
    }
    case Expression.ETuple(one, two) => {
      for {
        oneResult <- synthesizesTo(context, one)
        (oneType, gamma) = oneResult
        twoResult <- synthesizesTo(gamma, two)
        (twoType, delta) = twoResult
      } yield (Type.TProduct(oneType, twoType), delta)
    }
    case Expression.ELet(name, expr, body) => {
      for {
        expressionResult <- synthesizesTo(context, expr)
        (exprType, gamma) = expressionResult
        exprVariable = ContextElement.TypedVariable(name, exprType)
        theta = gamma.add(exprVariable)
        bodyResult <- synthesizesTo(theta, body)
        (bodyType, phi) = bodyResult
        delta <- phi.insertInPlace(exprVariable, List())
      } yield (bodyType, delta)
    }
    //->E
    case Expression.EApplication(fun, arg) => {
      for {
        funResult <- synthesizesTo(context, fun)
        (funType, theta) = funResult
        appResult <- applicationSynthesizesTo(
          theta,
          applyContext(funType, theta),
          arg
        )
      } yield appResult
    }
  }

//Figure 11
def applicationSynthesizesTo(
    context: Context,
    _type: Type,
    expr: Expression
): Eff[(Type, Context)] =
  _type match {
    //alphaApp
    case Type.TExistential(name) => {
      for {
        alpha1 <- MutableState.makeExistential
        alpha2 <- MutableState.makeExistential
        gamma <- context.insertInPlace(
          ContextElement.Existential(name),
          List(
            ContextElement.Existential(alpha2),
            ContextElement.Existential(alpha1),
            ContextElement.Solved(
              name,
              Type.TFunction(
                Type.TExistential(alpha1),
                Type.TExistential(alpha2)
              )
            )
          )
        )
        delta <- checksAgainst(gamma, expr, Type.TExistential(alpha1))
      } yield (Type.TExistential(alpha2), delta)
    }
    //ForallApp
    case Type.TQuantification(name, quantType) => {
      for {
        alpha <- MutableState.makeExistential
        gamma = context.add(ContextElement.Existential(alpha))
        substitutedType = substitution(
          quantType,
          name,
          Type.TExistential(alpha)
        )
        result <- applicationSynthesizesTo(gamma, substitutedType, expr)
      } yield result
    }
    //App
    case Type.TFunction(arg, ret) =>
      for {
        delta <- checksAgainst(context, expr, arg)
      } yield (ret, delta)
    case _ => fail(CannotApplyType(_type))
  }

/// Substitution is written in the paper as [α^/α]A which means, α is replaced with α^ in all occurrences in A
def substitution(a: Type, alpha: String, b: Type): Type = {
  a match {
    case _: Type.TLiteral     => a
    case Type.TVariable(name) => if (name == alpha) b else a
    case Type.TQuantification(name, quantType) => {
      if (name == alpha) {
        Type.TQuantification(name, b)
      } else {
        Type.TQuantification(name, substitution(quantType, alpha, b))
      }
    }
    case Type.TExistential(name) => if (name == alpha) b else a
    case Type.TProduct(one, two) =>
      Type.TProduct(
        substitution(one, alpha, b),
        substitution(two, alpha, b)
      )
    case Type.TFunction(arg, ret) =>
      Type.TFunction(
        substitution(arg, alpha, b),
        substitution(ret, alpha, b)
      )
  }
}

def synth(expr: Expression): Eff[Type] = {
  for {
    synthResult <- synthesizesTo(Context(), expr)
    (resultType, context) = synthResult
    result = applyContext(resultType, context)
  } yield result
}

def assertTrue[E](cond: Boolean, ifFail: => E): IO[E, Unit] =
  if (cond) {
    ZIO.unit
  } else {
    ZIO.fail(ifFail)
  }

/// Figure 7
def isWellFormed(context: Context, _type: Type): Boolean = {
  _type match {
    case _: Type.TLiteral     => true
    case Type.TVariable(name) => context.hasVariable(name)
    case Type.TFunction(arg, ret) =>
      isWellFormed(context, arg) && isWellFormed(context, ret)
    case Type.TQuantification(alpha, a) =>
      isWellFormed(context.add(ContextElement.Variable(alpha)), a)
    case Type.TExistential(name) =>
      context.hasExistential(name) || context.getSolved(name).isDefined
    case Type.TProduct(one, two) =>
      isWellFormed(context, one) && isWellFormed(context, two)
  }
}

/// This corresponds to the FV call in Figure 9 Rule <:InstantiateL and <:InstantiateR
/// It checks if a existential variable already occurs in a type to be able to find and panic on cycles
///
/// Alas, I could not find a definition of the FV function and had to copy the implementation of
/// https://github.com/ollef/Bidirectional and https://github.com/atennapel/bidirectional.js
def occursIn(alpha: String, a: Type): Boolean = {
  a match {
    case Type.TLiteral(_)     => false
    case Type.TVariable(name) => alpha == name
    case Type.TFunction(arg, ret) =>
      occursIn(alpha, arg) || occursIn(alpha, ret)
    case Type.TQuantification(beta, t) => {
      if (alpha == beta) {
        return true;
      } else {
        return occursIn(alpha, t);
      }
    }
    case Type.TExistential(name) => alpha == name
    case Type.TProduct(one, two) => occursIn(alpha, one) || occursIn(alpha, two)
  }
}

def subtype(context: Context, a: Type, b: Type): Eff[Context] =
  for {
    _ <- assertTrue(isWellFormed(context, a), TypeNotWellFormed(context, a))
    _ <- assertTrue(isWellFormed(context, b), TypeNotWellFormed(context, b))
    delta <- (a, b) match {
      //<:Unit
      case (Type.TLiteral(literalA), Type.TLiteral(literalB)) => {
        assertTrue(literalA == literalB, TypesNotEqual(a, b))
          .as(context)
      }
      //<:Var
      case (Type.TVariable(nameA), Type.TVariable(nameB)) => {
        assertTrue(isWellFormed(context, a), TypeNotWellFormed(context, a)) *>
          assertTrue(nameA == nameB, TypeNamesNotEqual(nameA, nameB))
            .as(context)
      }
      //<:Exvar
      case (Type.TExistential(name1), Type.TExistential(name2))
          if name1 == name2 => {
        assertTrue(isWellFormed(context, a), TypeNotWellFormed(context, a)).as(
          context
        )
      }
      //<:->
      case (Type.TFunction(arg1, ret1), Type.TFunction(arg2, ret2)) => {
        for {
          theta <- subtype(context, arg1, arg2)
          delta <- subtype(
            theta,
            applyContext(ret1, theta),
            applyContext(ret2, theta)
          )
        } yield delta
      }
      case (Type.TProduct(one1, two1), Type.TProduct(one2, two2)) => {
        subtype(context, one1, one2).flatMap(subtype(_, two1, two2))
      }
      //<:forallL
      case (Type.TQuantification(name, quantType), _) => {
        for {
          alpha <- MutableState.makeExistential
          gamma = context
            .add(ContextElement.Marker(alpha))
            .add(ContextElement.Existential(alpha))
          substitutedQuantType = substitution(
            quantType,
            name,
            Type.TExistential(alpha)
          )
          delta <- subtype(gamma, substitutedQuantType, b)
          result <- delta.drop(ContextElement.Marker(alpha))
        } yield result
      }
      //<:forallR
      case (_, Type.TQuantification(name, quantType)) => {
        val theta = context.add(ContextElement.Variable(name))
        subtype(theta, a, quantType)
          .flatMap(_.drop(ContextElement.Variable(name)))
      }
      //<:InstatiateL
      case (Type.TExistential(name), _) => {
        if (!occursIn(name, b)) {
          instantiateL(context, name, b)
        } else {
          fail(CircularInstantiation(context, name, b))
        }
      }
      //<:InstantiateR
      case (_, Type.TExistential(name)) => {
        if (!occursIn(name, a)) {
          instantiateR(context, a, name)
        } else {
          fail(CircularInstantiation(context, name, a))
        }
      }
    }
  } yield delta

/// Figure 10
def instantiateL(context: Context, alpha: String, b: Type): Eff[Context] = {
  for {
    split <- context.splitAt(ContextElement.Existential(alpha))
    (left, right) = split
    //InstLSolve
    result <-
      if (b.isMonotype && isWellFormed(left, b)) {
        context.insertInPlace(
          ContextElement.Existential(alpha),
          List(ContextElement.Solved(alpha, b))
        )
      } else {
        b match {
          //InstLArr
          case Type.TFunction(arg, ret) => {
            for {
              alpha1 <- MutableState.makeExistential
              alpha2 <- MutableState.makeExistential
              gamma <- context.insertInPlace(
                ContextElement.Existential(alpha),
                List(
                  ContextElement.Existential(alpha2),
                  ContextElement.Existential(alpha1),
                  ContextElement.Solved(
                    alpha,
                    Type.TFunction(
                      Type.TExistential(alpha1),
                      Type.TExistential(alpha2)
                    )
                  )
                )
              )
              theta <- instantiateR(gamma, arg, alpha1)
              delta <- instantiateL(theta, alpha2, applyContext(ret, theta))
            } yield delta
          }
          //InstAIIR
          case Type.TQuantification(beta, b) => {
            for {
              gamma <- instantiateL(
                context.add(ContextElement.Variable(beta)),
                alpha,
                b
              )
              delta <- gamma.drop(ContextElement.Variable(beta))
            } yield delta
          }
          //InstLReach
          case Type.TExistential(beta) => {
            context.insertInPlace(
              ContextElement.Existential(beta),
              List(ContextElement.Solved(beta, Type.TExistential(alpha)))
            )
          }
          case _ => fail(CannotInstantiateL(context, alpha, b))
        }
      }
  } yield result
}

/// Figure 10
def instantiateR(context: Context, a: Type, alpha: String): Eff[Context] =
  for {
    split <- context.splitAt(ContextElement.Existential(alpha))
    (left, right) = split
    result <-
      if (a.isMonotype && isWellFormed(left, a)) {
        //InstRSolve
        context.insertInPlace(
          ContextElement.Existential(alpha),
          List(ContextElement.Solved(alpha, a))
        )
      } else {
        a match {
          //InstRArr
          case Type.TFunction(arg, ret) => {
            for {
              alpha1 <- MutableState.makeExistential
              alpha2 <- MutableState.makeExistential
              gamma <- context.insertInPlace(
                ContextElement.Existential(alpha),
                List(
                  ContextElement.Existential(alpha2),
                  ContextElement.Existential(alpha1),
                  ContextElement.Solved(
                    alpha,
                    Type.TFunction(
                      Type.TExistential(alpha1),
                      Type.TExistential(alpha2)
                    )
                  )
                )
              )
              theta <- instantiateL(gamma, alpha1, arg)
              delta <- instantiateR(theta, applyContext(ret, theta), alpha2)
            } yield delta
          }
          //InstRAllL
          case Type.TQuantification(beta, b) => {
            for {
              beta1 <- MutableState.makeExistential
              gamma = context
                .add(ContextElement.Marker(beta1))
                .add(ContextElement.Existential(beta1))
              theta <- instantiateR(
                gamma,
                substitution(b, beta, Type.TExistential(beta1)),
                alpha
              )
              delta <- theta.drop(ContextElement.Marker(beta1))
            } yield delta
          }
          case Type.TProduct(one, two) => {
            for {
              alpha1 <- MutableState.makeExistential
              beta1 <- MutableState.makeExistential
              gamma <- context.insertInPlace(
                ContextElement.Existential(alpha),
                List(
                  ContextElement.Existential(beta1),
                  ContextElement.Existential(alpha1),
                  ContextElement.Solved(
                    alpha,
                    Type.TProduct(
                      Type.TExistential(alpha1),
                      Type.TExistential(beta1)
                    )
                  )
                )
              )
              theta <- instantiateL(gamma, alpha1, one)
              delta <- instantiateR(theta, applyContext(two, theta), beta1)
            } yield delta
          }
          //InstRReach
          case Type.TExistential(beta) => {
            context.insertInPlace(
              ContextElement.Existential(beta),
              List(ContextElement.Solved(beta, Type.TExistential(alpha)))
            )
          }
          case _ => fail(CannotInstantiateR(context, alpha, a))
        }
      }
  } yield result

/// Figure 8
def applyContext(_type: Type, context: Context): Type = {
  _type match {
    case Type.TLiteral(_)  => _type
    case Type.TVariable(_) => _type
    case Type.TExistential(name) => {
      context.getSolved(name).fold(_type)(applyContext(_, context))
    }
    case Type.TFunction(argType, returnType) =>
      Type.TFunction(
        applyContext(argType, context),
        applyContext(returnType, context)
      )
    case Type.TQuantification(name, quantType) => {
      Type.TQuantification(name, applyContext(quantType, context))
    }
    case Type.TProduct(oneType, twoType) =>
      Type.TProduct(
        applyContext(oneType, context),
        applyContext(twoType, context)
      )
  }
}

object App extends zio.App {
  val program: ZIO[Env, Throwable, Unit] = ZIO.unit
  def run(args: List[String]): zio.URIO[zio.ZEnv, zio.ExitCode] =
    (program.provideSomeLayer[ZEnv](MutableState.live) *> putStrLn(
      "jek"
    )).exitCode
}

@main def hello: Unit = {
  val c = Context(elements =
    Vector(
      ContextElement.Solved("name1", Type.TLiteral(LiteralType.LTChar)),
      ContextElement.Solved("name3", Type.TLiteral(LiteralType.LTBool))
    )
  )
  println(c.getSolved("name3"))
  println(msg)
}

def msg = "I was compiled by Scala 3. :)"
