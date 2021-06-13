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

// Checks if any element of the collection satisfies the given effectful predicate
def anyM[R, E, A](
    collection: Iterable[A],
    test: A => ZIO[R, E, Boolean]
): ZIO[R, E, Boolean] =
  ZIO.foldLeft(collection)(false)((res, elem) =>
    if (res) ZIO.succeed(res)
    else test(elem)
  )
