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

def canSolve(context: Context, _type: Type): Eff[Boolean] = {
  if (_type.isMonotype) {
    checkIsWellFormed(context, _type).isSuccess
  } else {
    succeed(false)
  }
}

// Fig 10
def instantiateL(context: Context, alpha: String, b: Type): Eff[Context] = {
  for {
    (left, right) <- context.splitAt(CExistential(alpha))
    //InstLSolve
    result <-
      ZIO.ifM(canSolve(left, b))(
        onTrue = context.insertInPlace(
          CExistential(alpha),
          List(CSolved(alpha, b))
        ),
        onFalse = b match {
          //InstLArr
          case TFunction(args, returnType) => {
            for {
              retAlpha  <- CompilerState.makeExistential
              argAlphas <- ZIO.foreach(args)(_ => CompilerState.makeExistential)
              gamma <- context.insertInPlace(
                CExistential(alpha),
                CExistential(retAlpha) ::
                  // reverse to place the first argument α^ to the right in the context
                  argAlphas.reverse.map(CExistential(_)) :::
                  List(
                    CSolved(
                      alpha,
                      TFunction(
                        argAlphas.map(TExistential(_)),
                        TExistential(retAlpha)
                      )
                    )
                  )
              )
              theta <- ZIO.foldLeft(argAlphas.zip(args))(gamma) {
                case (delta, (alphaN, argType)) =>
                  instantiateR(delta, alphaN, argType)
              }
              appliedReturnType <- applyContext(returnType, theta)
              delta <- instantiateL(theta, retAlpha, appliedReturnType)
            } yield delta
          }
          //InstAIIR
          case TQuantification(beta, b) => {
            for {
              theta <- context.add(CVariable(beta))
              gamma <- instantiateL(theta, alpha, b)
              delta <- gamma.drop(CVariable(beta))
            } yield delta
          }
          case it: TMulQuantification =>
            instantiateL(context, alpha, it.desugar)
          //InstLReach
          case TExistential(beta) => {
            context.insertInPlace(
              CExistential(beta),
              List(CSolved(beta, TExistential(alpha)))
            )
          }
          case _ => fail(CannotInstantiateL(context, alpha, b))
        }
      )

  } yield result
}

/// Fig 10
def instantiateR(context: Context, alpha: String, a: Type): Eff[Context] =
  for {
    (left, right) <- context.splitAt(CExistential(alpha))
    result <-
      ZIO.ifM(canSolve(left, a))(
        //InstRSolve
        onTrue = context.insertInPlace(
          CExistential(alpha),
          List(CSolved(alpha, a))
        ),
        onFalse = a match {
          //InstRArr
          case TFunction(args, returnType) => {
            for {
              retAlpha  <- CompilerState.makeExistential
              argAlphas <- ZIO.foreach(args)(_ => CompilerState.makeExistential)
              gamma <- context.insertInPlace(
                CExistential(alpha),
                CExistential(retAlpha) ::
                  // reverse to place the first argument α^ to the right in the context
                  argAlphas.reverse.map(CExistential(_)) :::
                  List(
                    CSolved(
                      alpha,
                      TFunction(
                        argAlphas.map(TExistential(_)),
                        TExistential(retAlpha)
                      )
                    )
                  )
              )
              theta <- ZIO.foldLeft(argAlphas.zip(args))(gamma) {
                case (delta, (alphaN, argType)) =>
                  instantiateL(delta, alphaN, argType)
              }
              appliedReturnType <- applyContext(returnType, theta)
              delta <- instantiateR(theta, retAlpha, appliedReturnType)
            } yield delta
          }
          //InstRAllL
          case TQuantification(beta, b) => {
            for {
              beta1 <- CompilerState.makeExistential
              gamma <- context
                .addAll(CMarker(beta1), CExistential(beta1))
              substituted <- substitution(context, b, beta, TExistential(beta1))
              theta <- instantiateR(
                gamma,
                alpha,
                substituted
              )
              delta <- theta.drop(CMarker(beta1))
            } yield delta
          }
          case it: TMulQuantification =>
            instantiateR(context, alpha, it.desugar)
          //InstRReach
          case TExistential(beta) => {
            context.insertInPlace(
              CExistential(beta),
              List(CSolved(beta, TExistential(alpha)))
            )
          }
          case _ => fail(CannotInstantiateR(context, alpha, a))
        }
      )
  } yield result
