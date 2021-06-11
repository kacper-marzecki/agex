import cats.implicits.*
import zio.*
import ZIO.{fail, succeed}
import AppError.*
import ContextElement.*
import Type.*

type AddResult[A <: ContextElement] = A match {
  case CTypedVariable => IO[ShadowedVariableName, Context]
  case ?              => Context
}

case class Context(elements: Vector[ContextElement] = Vector.empty) {
  lazy val names = elements.mapFilter { it =>
    it match {
      case _: CTypedVariable => Some(it.name)
      case _                 => None
    }
  }.toSet

  /** Watches for name shadowing in the current context
    *
    * TODO: fix dirty hack - remove casting to AddResult[A].
    *
    * I don't know why, but the compiler doesn't think we return the
    * AddResult[A] match type.
    * https://dotty.epfl.ch/docs/reference/new-types/match-types.html
    */
  def add[A <: ContextElement](element: A): AddResult[A] = {
    element match {
      case _: CTypedVariable =>
        if (names.contains(element.name)) {
          fail(
            ShadowedVariableName(this, element.name)
          )
            .asInstanceOf[AddResult[A]]
        } else {
          succeed(this.copy(elements = elements.appended(element)))
            .asInstanceOf[AddResult[A]]
        }
      case _ =>
        this
          .copy(elements = elements.appended(element))
          .asInstanceOf[AddResult[A]]
    }
  }
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

/// Fig 7
def isWellFormed(context: Context, _type: Type): Boolean = {
  _type match {
    case _: TLiteral     => true
    case TVariable(name) => context.hasVariable(name)
    case TFunction(arg, ret) =>
      isWellFormed(context, arg) && isWellFormed(context, ret)
    case TQuantification(alpha, a) =>
      isWellFormed(context.add(CVariable(alpha)), a)
    case TExistential(name) =>
      context.hasExistential(name) || context.getSolved(name).isDefined
    case TTuple(valueTypes) =>
      valueTypes.forall(isWellFormed(context, _))
  }
}

// Fig 8
def applyContext(_type: Type, context: Context): Type = {
  _type match {
    case TLiteral(_)  => _type
    case TVariable(_) => _type
    case TExistential(name) => {
      context.getSolved(name).fold(_type)(applyContext(_, context))
    }
    case TFunction(argType, returnType) =>
      TFunction(
        applyContext(argType, context),
        applyContext(returnType, context)
      )
    case TQuantification(name, quantType) => {
      TQuantification(name, applyContext(quantType, context))
    }
    case TTuple(valueTypes) =>
      TTuple(valueTypes.map(applyContext(_, context)))
  }
}
