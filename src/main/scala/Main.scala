import cats.implicits.*
import zio.*
import zio.console.{putStrLn, putStrLnErr}
import cats.syntax.apply
import ZIO.{succeed, fail}
import AppError.*

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
    case (Expression.ELiteral(literal), Type.Literal(_type)) => {
      assertLiteralChecksAgainst(literal, _type).as(context)
    }
    //->I
    case (
          Expression.EAbstraction(arg, body),
          Type.Function(argType, bodyType)
        ) => {
      val typedVar = ContextElement.TypedVariable(arg, argType)
      val gamma = context.add(typedVar)
      checksAgainst(gamma, body, bodyType)
        .flatMap(_.drop(typedVar))
    }
    case (expression, Type.Quantification(name, quantType)) => {
      val variable = ContextElement.Variable(name)
      val gamma = context.add(variable)
      checksAgainst(gamma, expression, quantType).flatMap(_.drop(variable))
    }
    //xI
    case (Expression.ETuple(one, two), Type.Product(oneType, twoType)) => {
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
      succeed((Type.Literal(literalSynthesizesTo(literal)), context))
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
          .add(ContextElement.TypedVariable(arg, Type.Existential(alpha)))
        delta <- checksAgainst(gamma, ret, Type.Existential(beta))
          .flatMap(
            _.drop(ContextElement.TypedVariable(arg, Type.Existential(alpha)))
          )
      } yield (
        Type.Function(Type.Existential(alpha), Type.Existential(beta)),
        delta
      )
    }
    case Expression.ETuple(one, two) => {
      for {
        oneResult <- synthesizesTo(context, one)
        (oneType, gamma) = oneResult
        twoResult <- synthesizesTo(gamma, two)
        (twoType, delta) = twoResult
      } yield (Type.Product(oneType, twoType), delta)
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
        argResult <- synthesizesTo(context, arg)
        (argType, theta) = argResult
        appResult <- applicationSynthesizesTo(
          theta,
          applyContext(argType, theta),
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
    case Type.Existential(name) => {
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
              Type.Function(Type.Existential(alpha1), Type.Existential(alpha2))
            )
          )
        )
        delta <- checksAgainst(gamma, expr, Type.Existential(alpha1))
      } yield (Type.Existential(alpha2), delta)
    }
    //ForallApp
    case Type.Quantification(name, quantType) => {
      for {
        alpha <- MutableState.makeExistential
        gamma = context.add(ContextElement.Existential(alpha))
        substitutedType = substitution(
          quantType,
          name,
          Type.Existential(alpha)
        )
        result <- applicationSynthesizesTo(gamma, substitutedType, expr)
      } yield result
    }
    //App
    case Type.Function(arg, ret) =>
      for {
        delta <- checksAgainst(context, expr, arg)
      } yield (ret, delta)
    case _ => fail(CannotApplyType(_type))
  }

def substitution(a: Type, alpha: String, b: Type): Type = ???

def assertTrue[E](cond: Boolean, ifFail: => E): IO[E, Unit] =
  if (cond) {
    ZIO.unit
  } else {
    ZIO.fail(ifFail)
  }

/// Figure 7
def isWellFormed(context: Context, _type: Type): Boolean = {
  _type match {
    case _: Type.Literal     => true
    case Type.Variable(name) => context.hasVariable(name)
    case Type.Function(arg, ret) =>
      isWellFormed(context, arg) && isWellFormed(context, ret)
    case Type.Quantification(alpha, a) =>
      isWellFormed(context.add(ContextElement.Variable(alpha)), a)
    case Type.Existential(name) =>
      context.hasExistential(name) || context.getSolved(name).isDefined
    case Type.Product(one, two) =>
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
    case Type.Literal(_)         => false
    case Type.Variable(name)     => alpha == name
    case Type.Function(arg, ret) => occursIn(alpha, arg) || occursIn(alpha, ret)
    case Type.Quantification(beta, t) => {
      if (alpha == beta) {
        return true;
      } else {
        return occursIn(alpha, t);
      }
    }
    case Type.Existential(name) => alpha == name
    case Type.Product(one, two) => occursIn(alpha, one) || occursIn(alpha, two)
  }
}

def subtype(context: Context, a: Type, b: Type): Eff[Context] =
  for {
    _ <- assertTrue(isWellFormed(context, a), TypeNotWellFormed(context, a))
    _ <- assertTrue(isWellFormed(context, b), TypeNotWellFormed(context, b))
    delta <- (a, b) match {
      //<:Unit
      case (Type.Literal(literalA), Type.Literal(literalB)) => {
        assertTrue(literalA == literalB, TypesNotEqual(a, b))
          .as(context)
      }
      //<:Var
      case (Type.Variable(nameA), Type.Variable(nameB)) => {
        assertTrue(isWellFormed(context, a), TypeNotWellFormed(context, a)) *>
          assertTrue(nameA == nameB, TypeNamesNotEqual(nameA, nameB))
            .as(context)
      }
      //<:Exvar
      case (Type.Existential(name1), Type.Existential(name2))
          if name1 == name2 => {
        assertTrue(isWellFormed(context, a), TypeNotWellFormed(context, a)).as(
          context
        )
      }
      //<:->
      case (Type.Function(arg1, ret1), Type.Function(arg2, ret2)) => {
        for {
          theta <- subtype(context, arg1, arg2)
          delta <- subtype(
            theta,
            applyContext(ret1, theta),
            applyContext(ret2, theta)
          )
        } yield delta
      }
      case (Type.Product(one1, two1), Type.Product(one2, two2)) => {
        subtype(context, one1, one2).flatMap(subtype(_, two1, two2))
      }
      //<:forallL
      case (Type.Quantification(name, quantType), _) => {
        for {
          alpha <- MutableState.makeExistential
          gamma = context
            .add(ContextElement.Marker(alpha))
            .add(ContextElement.Existential(alpha))
          substitutedQuantType = substitution(
            quantType,
            name,
            Type.Existential(alpha)
          )
          delta <- subtype(gamma, substitutedQuantType, b)
          result <- delta.drop(ContextElement.Marker(alpha))
        } yield result
      }
      //<:forallR
      case (_, Type.Quantification(name, quantType)) => {
        val theta = context.add(ContextElement.Variable(name))
        subtype(theta, a, quantType)
          .flatMap(_.drop(ContextElement.Variable(name)))
      }
      //<:InstatiateL
      case (Type.Existential(name), _) => {
        if (!occursIn(name, b)) {
          instantiateL(context, name, b)
        } else {
          fail(CircularInstantiation(context, name, b))
        }
      }
      //<:InstantiateR
      case (_, Type.Existential(name)) => {
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
        succeed(
          context.insertInPlace(
            ContextElement.Existential(alpha),
            List(ContextElement.Solved(alpha, b))
          )
        )
      } else {
        b match {
          //InstLArr
          case Type.Function(arg, ret) => {
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
                    Type.Function(
                      Type.Existential(alpha1),
                      Type.Existential(alpha2)
                    )
                  )
                )
              )
              theta <- instantiateR(gamma, arg, alpha1)
              delta <- instantiateL(theta, alpha2, applyContext(ret, theta))
            } yield ???
          }
          //InstAIIR
          case Type.Quantification(beta, b) => {
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
          case Type.Existential(beta) => {
            context.insertInPlace(
              ContextElement.Existential(beta),
              List(ContextElement.Solved(beta, Type.Existential(alpha)))
            )
          }
          case _ => fail(CannotInstantiate(context, alpha, b))
        }
      }
  } yield ???
}

/// Figure 10
def instantiateR(context: Context, a: Type, alpha: String): Eff[Context] =
  ???

/// Figure 8
def applyContext(_type: Type, context: Context): Type = {
  _type match {
    case Type.Literal(_)  => _type
    case Type.Variable(_) => _type
    case Type.Existential(name) => {
      context.getSolved(name).fold(_type)(applyContext(_, context))
    }
    case Type.Function(argType, returnType) =>
      Type.Function(
        applyContext(argType, context),
        applyContext(returnType, context)
      )
    case Type.Quantification(name, quantType) => {
      Type.Quantification(name, applyContext(quantType, context))
    }
    case Type.Product(oneType, twoType) =>
      Type.Product(
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
      ContextElement.Solved("name1", Type.Literal(LiteralType.LTChar)),
      ContextElement.Solved("name3", Type.Literal(LiteralType.LTBool))
    )
  )
  println(c.getSolved("name3"))
  println(msg)
}

def msg = "I was compiled by Scala 3. :)"
