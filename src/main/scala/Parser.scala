import Expression.*
import Literal.*
import cats.parse.{Parser0 as P0, Parser as P, Numbers}
import cats.data.NonEmptyList

sealed trait SExpToken
case class SId(value: String)               extends SExpToken
case class SString(value: String)           extends SExpToken
case class SExpr(elements: List[SExpToken]) extends SExpToken
case class SInt(value: String)              extends SExpToken
case class SFloat(value: String)            extends SExpToken
case class SAtom(value: String)             extends SExpToken

object Parser {
  val lowercaseAlphabet = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',
    'k', 'l', 'm', 'n', 'o', 'p', 'r', 's', 't', 'u', 'w', 'y', 'v', 'z')
  val uppercaseAlphabet = lowercaseAlphabet.map(_.toUpper)
  val specialIdChars    = List('?', '!', '&', '*', '/', '+', '-', '%', ':', '=')
  val numbers           = (1 to 9).map(_.toString.charAt(0)).toList
  val pWhitespace: P[Unit]   = P.charIn(" \t\r\n").void
  val whitespaces0: P0[Unit] = pWhitespace.rep0.void
  // TODO differentiate between characters that are allowed on the first and n-th place in the identifier
  val pInt =
    P.charIn(numbers)
      .rep
      .map(it => SInt(it.toList.mkString))
      .surroundedBy(whitespaces0)
  val pFloat =
    (P.charIn(numbers).rep ~ P.char('.') ~ P.charIn(numbers).rep)
      .map { case ((before, point), after) =>
        SFloat(s"${before.toList.mkString}.${after.toList.mkString}")
      }
      .surroundedBy(whitespaces0)
  val pAtom = (P.char(':') *> P
    .charIn(
      lowercaseAlphabet ::: uppercaseAlphabet ::: specialIdChars ::: numbers
    )
    .rep).map(it => SAtom(it.toList.mkString))
  val pIdentifier =
    (P.charIn(lowercaseAlphabet ::: uppercaseAlphabet ::: specialIdChars) ~ P
      .charIn(
        lowercaseAlphabet ::: uppercaseAlphabet ::: specialIdChars ::: numbers
      )
      .rep0)
      .surroundedBy(whitespaces0)
      .map { case (first, rest) => SId((first :: rest.toList).mkString) }
  val justStr = JsonStringUtil.escapedString('"')
  val pString = justStr.map(SString(_))

  val pExpr = P.recursive[SExpToken] { r =>
    (P.char('(') *> P
      .oneOf(pString :: pIdentifier :: r :: pFloat.backtrack :: pInt :: Nil)
      .surroundedBy(whitespaces0)
      .rep
      <* P.char(')'))
      .map(it => SExpr(it.toList))
      .surroundedBy(whitespaces0)

  }
}
