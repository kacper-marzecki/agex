import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline}
import zio.*
import zio.console.{putStrLn, Console, putStr}

extension [A](it: Vector[A]) {
  def findIndexOf(elem: A) =
    it.indexWhere(_ == elem) match {
      case -1    => None
      case index => Some(index)
    }
}

def prettyShow(it: Any): String =
  pprint.tokenize(it).mkString("") + "\n"

def prettyPrint(it: Any): URIO[Console, Unit] =
  putStrLn(prettyShow(it)).orDie
