import cats.implicits.*
import zio.*
import ZIO.{fail, succeed}
import AppError.*
import ContextElement.*
type AddResult[A <: ContextElement] = A match {
  case CTypedVariable => Either[ShadowedVariableName, Context]
  case ?              => Context
}

case class Context(elements: Vector[ContextElement] = Vector.empty) {
  lazy val names = elements.mapFilter {
    case tv: CTypedVariable => Some(tv.name)
    case _                  => None
  }.toSet

  def add(it: ContextElement) =
    this.copy(elements = elements.appended(it))

  // watch for name shadowing
  // def safeAdd[A <: ContextElement](element: A): AddResult[A] =
  //   element match {
  //     case it: CTypedVariable =>
  //       if (names.contains(it.name)) {
  //         Left[ShadowedVariableName, Context](
  //           ShadowedVariableName(this, it.name)
  //         ).asInstanceOf[AddResult[A]]
  //       } else {
  //         Right[ShadowedVariableName, Context](
  //           this.copy(elements = elements.appended(it))
  //         ).asInstanceOf[AddResult[A]]
  //       }
  //     case _ =>
  //       this
  //         .copy(elements = elements.appended(element))
  //         .asInstanceOf[AddResult[A]]
  //   }

  def splitAt(it: ContextElement): IO[ElementNotFound, (Context, Context)] = {
    elements.findIndexOf(it) match {
      case None => fail(ElementNotFound(this, it))
      case Some(index) =>
        val (l, r) = elements.splitAt(index)
        succeed((Context(l), Context(r)))
    }
  }

  def insertInPlace(
      element: ContextElement,
      inserts: List[ContextElement]
  ): IO[ElementNotFound, Context] =
    for {
      newElements <- elements.findIndexOf(element) match {
        case None => fail[ElementNotFound](ElementNotFound(this, element))
        case Some(index) => succeed(elements.patch(index, inserts, 1))
      }
    } yield Context(newElements)

  def drop(
      element: ContextElement
  ): IO[ElementNotFound, Context] = {
    elements.findIndexOf(element) match {
      case None        => ZIO.fail(ElementNotFound(this, element))
      case Some(index) => ZIO.succeed(Context(elements.splitAt(index)._1))
    }
  }

  def getSolved(name: String): Option[Type] =
    elements.mapFilter {
      case CSolved(name1, _type) if name == name1 => Some(_type)
      case _                                      => None
    }.headOption

  def hasExistential(name: String): Boolean =
    elements.contains(CExistential(name))

  // only for quantifiations
  def hasVariable(name: String): Boolean =
    elements.contains(CVariable(name))

  def getAnnotation(name: String): Option[Type] =
    elements.mapFilter {
      case CTypedVariable(name1, _type) if name == name1 =>
        Some(_type)
      case _ => None
    }.headOption
}
