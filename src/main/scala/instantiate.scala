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

// Fig 10
def instantiateL(context: Context, alpha: String, b: Type): Eff[Context] = {
  for {
    (left, right) <- context.splitAt(CExistential(alpha))
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
          case TFunction(arg, returnType) => {
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
                    TFunction(TExistential(alpha1), TExistential(alpha2))
                  )
                )
              )
              theta <- instantiateR(gamma, alpha1, arg)
              delta <- instantiateL(
                theta,
                alpha2,
                applyContext(returnType, theta)
              )
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
def instantiateR(context: Context, alpha: String, a: Type): Eff[Context] =
  for {
    (left, right) <- context.splitAt(CExistential(alpha))
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
              delta <- instantiateR(theta, alpha2, applyContext(ret, theta))
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
                alpha,
                substitution(b, beta, TExistential(beta1))
              )
              delta <- theta.drop(CMarker(beta1))
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
