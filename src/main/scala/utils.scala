import zio.*
import zio.console.{putStrLn, Console, putStr}
import cats.implicits.*
import zio.interop.catz.*

extension [A](it: Vector[A]) {
  def findIndexOf(elem: A) =
    it.lastIndexWhere(_ == elem) match {
      case -1    => None
      case index => Some(index)
    }
}

def prettyShow(it: Any): String =
  pprint.tokenize(it).mkString("") + "\n"

def pPrint(it: Any, tag: String) =
  ZIO(pprint.pprintln(tag)).orDie *> ZIO(pprint.pprintln(it)).orDie

def prettyPrint(it: Any, tag: String): URIO[Console, Unit] =
  putStrLn(s"$tag: ${prettyShow(it)}").orDie

def repeat[A](xs: List[A], times: Int) =
  List.fill(times)(xs).flatten

def assertTrue[E](cond: Boolean, ifFail: => E): IO[E, Unit] =
  if (cond) {
    ZIO.unit
  } else {
    ZIO.fail(ifFail)
  }

def assertM[R, E](check: ZIO[R, E, Boolean], failure: => E) = {
  ZIO.ifM(check)(
    ZIO.unit,
    ZIO.fail(failure)
  )
}

def assertNotM[R, E](check: ZIO[R, E, Boolean], failure: => E) = {
  ZIO.ifM(check)(
    ZIO.fail(failure),
    ZIO.unit
  )
}

def findM[R, E, A, B](
    collection: Iterable[A],
    test: A => ZIO[R, E, B],
    ifNotFound: => E
): ZIO[R, E, B] =
  collection.toList.tailRecM {
    case Nil     => ZIO.fail(ifNotFound)
    case x :: xs => test(x).fold(_ => Left(xs), Right(_))
  }

// Checks if any element of the collection satisfies the given effectful predicate
def anyM[R, E, A](
    collection: Iterable[A],
    test: A => ZIO[R, E, Boolean]
): ZIO[R, E, Boolean] =
  ZIO.foldLeft(collection)(false)((res, elem) =>
    if (res) ZIO.succeed(res)
    else test(elem)
  )

case class ExtractionResult[A, B](
    included: Map[A, B],
    excluded: Map[A, B],
    notFound: Set[A]
)
def extractKeys[A, B](
    map: Map[A, B],
    keys: Iterable[A]
): ExtractionResult[A, B] = {
  val keySet   = keys.toSet
  val included = scala.collection.mutable.Map[A, B]()
  val excluded = scala.collection.mutable.Map[A, B]()
  val notFound = scala.collection.mutable.Set.from(keys)
  map.foreachEntry((k, v) => {
    if (keySet.contains(k)) {
      included.addOne((k, v))
      notFound.remove(k)
    } else {
      excluded.addOne((k, v));
    }
    ()
  })
  ExtractionResult(included.toMap, excluded.toMap, notFound.toSet)
}

def split[A, B](xs: List[(A, B)]): (List[A], List[B]) =
  xs.foldRight((List.empty[A], List.empty[B])) {
    case ((a, b), (accA, accB)) => {
      (a :: accA, b :: accB)
    }
  }

def loadFile(fileName: String) =
  ZIO(
    scala.io.Source
      .fromFile(fileName)
      .getLines
      .toList
      .foldSmash("", "\n", "")
  ).mapError(AppError.UnknownError(_))

def loadFilesInDirectory(directory: String) = {
  import java.io.File
  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }
  ZIO {
    recursiveListFiles(new File(directory)).map { f =>
      val source = scala.io.Source.fromFile(f)
      val lines  = source.getLines.toList.foldSmash("", "\n", "")
      source.close
      lines
    }.toList
  }.mapError(AppError.UnknownError(_))
}
