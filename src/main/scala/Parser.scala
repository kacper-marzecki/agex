import Expression.*
import Literal.*
import cats.parse.{Parser0 as P0, Parser as P, Numbers}
import cats.data.NonEmptyList

sealed trait SExpToken
case class SId(value: String)              extends SExpToken
case class SString(value: String)          extends SExpToken
case class SExp(elements: List[SExpToken]) extends SExpToken
case class SInt(value: String)             extends SExpToken
case class SFloat(value: String)           extends SExpToken
case class SAtom(value: String)            extends SExpToken

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
      .map(it => SExp(it.toList))
      .surroundedBy(whitespaces0)

  }
}

object JsonStringUtil extends GenericStringUtil {
  lazy val decodeTable: Map[Char, Char] =
    Map(
      ('\\', '\\'),
      ('\'', '\''),
      ('\"', '\"'),
      ('b', 8.toChar),  // backspace
      ('f', 12.toChar), // form-feed
      ('n', '\n'),
      ('r', '\r'),
      ('t', '\t')
    )
}

abstract class GenericStringUtil {
  protected def decodeTable: Map[Char, Char]

  private val encodeTable = decodeTable.iterator.map { case (v, k) =>
    (k, s"\\$v")
  }.toMap

  private val nonPrintEscape: Array[String] =
    (0 until 32).map { c =>
      val strHex = c.toHexString
      val strPad = List.fill(4 - strHex.length)('0').mkString
      s"\\u$strPad$strHex"
    }.toArray

  val escapedToken: P[Unit] = {
    val escapes = P.charIn(decodeTable.keys.toSeq)

    val oct  = P.charIn('0' to '7')
    val octP = P.char('o') ~ oct ~ oct

    val hex  = P.charIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F'))
    val hex2 = hex ~ hex
    val hexP = P.char('x') ~ hex2

    val hex4 = hex2 ~ hex2
    val u4   = P.char('u') ~ hex4
    val hex8 = hex4 ~ hex4
    val u8   = P.char('U') ~ hex8

    val after = P.oneOf[Any](escapes :: octP :: hexP :: u4 :: u8 :: Nil)
    (P.char('\\') ~ after).void
  }

  /** String content without the delimiter
    */
  def undelimitedString(endP: P[Unit]): P[String] =
    escapedToken.backtrack
      .orElse((!endP).with1 ~ P.anyChar)
      .rep
      .string
      .flatMap { str =>
        unescape(str) match {
          case Right(str1) => P.pure(str1)
          case Left(_)     => P.fail
        }
      }

  private val simpleString: P0[String] =
    P.charsWhile0(c => c >= ' ' && c != '"' && c != '\\')

  def escapedString(q: Char): P[String] = {
    val end: P[Unit] = P.char(q)
    end *> ((simpleString <* end).backtrack
      .orElse(undelimitedString(end) <* end))
  }

  def escape(quoteChar: Char, str: String): String = {
    // We can ignore escaping the opposite character used for the string
    // x isn't escaped anyway and is kind of a hack here
    val ignoreEscape =
      if (quoteChar == '\'') '"' else if (quoteChar == '"') '\'' else 'x'
    str.flatMap { c =>
      if (c == ignoreEscape) c.toString
      else
        encodeTable.get(c) match {
          case None =>
            if (c < ' ') nonPrintEscape(c.toInt)
            else c.toString
          case Some(esc) => esc
        }
    }
  }

  def unescape(str: String): Either[Int, String] = {
    val sb = new java.lang.StringBuilder
    def decodeNum(idx: Int, size: Int, base: Int): Int = {
      val end = idx + size
      if (end <= str.length) {
        val intStr = str.substring(idx, end)
        val asInt =
          try Integer.parseInt(intStr, base)
          catch { case _: NumberFormatException => ~idx }
        sb.append(asInt.toChar)
        end
      } else ~(str.length)
    }
    @annotation.tailrec
    def loop(idx: Int): Int =
      if (idx >= str.length) {
        // done
        idx
      } else if (idx < 0) {
        // error from decodeNum
        idx
      } else {
        val c0 = str.charAt(idx)
        if (c0 != '\\') {
          sb.append(c0)
          loop(idx + 1)
        } else {
          // str(idx) == \
          val nextIdx = idx + 1
          if (nextIdx >= str.length) {
            // error we expect there to be a character after \
            ~idx
          } else {
            val c = str.charAt(nextIdx)
            decodeTable.get(c) match {
              case Some(d) =>
                sb.append(d)
                loop(idx + 2)
              case None =>
                c match {
                  case 'o'   => loop(decodeNum(idx + 2, 2, 8))
                  case 'x'   => loop(decodeNum(idx + 2, 2, 16))
                  case 'u'   => loop(decodeNum(idx + 2, 4, 16))
                  case 'U'   => loop(decodeNum(idx + 2, 8, 16))
                  case other =>
                    // \c is interpretted as just \c, if the character isn't escaped
                    sb.append('\\')
                    sb.append(other)
                    loop(idx + 2)
                }
            }
          }
        }
      }

    val res = loop(0)
    if (res < 0) Left(~res)
    else Right(sb.toString)
  }
}
