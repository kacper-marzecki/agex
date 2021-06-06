import cats.implicits.*
import zio.*
import zio.console.{putStrLn, putStrLnErr}
import cats.syntax.apply

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

// TODO rename
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
        .map(_.drop(typedVar))
    }
    case (expression, Type.Quantification(name, quantType)) => {
      val variable = ContextElement.Variable(name)
      val gamma = context.add(variable)
      checksAgainst(gamma, expression, quantType).map(_.drop(variable))
    }
    //xI
    case (Expression.ETuple(one, two), Type.Product(oneType, twoType)) => {
      checksAgainst(context, one, oneType).flatMap(
        checksAgainst(_, two, twoType)
      )
    }
    //Sub
    case (_, _) => {
      synthesizesTo(context, expr)
        .flatMap { case (a, theta) =>
          subtype(
            theta,
            applyContext(a, theta),
            applyContext(_type, theta)
          )
        }
    }
    // TODO meaningful error message
    case _ => ???
  }
}

def synthesizesTo(context: Context, expr: Expression): Eff[(Type, Context)] =
  ???
def subtype(context: Context, a: Type, b: Type): Eff[Context] =
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
  val program: ZIO[Env, Throwable, Unit] = ZIO.succeed(())
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
