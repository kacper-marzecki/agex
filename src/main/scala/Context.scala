import cats.implicits.*
import zio.*
import ZIO.{fail, succeed}
import AppError.*
import ContextElement.*
import Type.*
import cats.syntax.apply

type AddResult[A <: ContextElement] = A match {
  case CTypedVariable  => IO[ShadowedVariableName, Context]
  case CTypeDefinition => IO[TypeWithNameAlreadyExists, Context]
  case ?               => Context
}

case class Context(elements: Vector[ContextElement] = Vector.empty) {
  lazy val typedVariableNames = elements.mapFilter { it =>
    it match {
      case _: CTypedVariable => Some(it.name)
      case _                 => None
    }
  }.toSet

  lazy val typeDefinitions = elements.mapFilter { it =>
    it match {
      case _: CTypeDefinition => Some(it.name)
      case _                  => None
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
        if (typedVariableNames.contains(element.name)) {
          fail(
            ShadowedVariableName(this, element.name)
          )
            .asInstanceOf[AddResult[A]]
        } else {
          succeed(Context(elements = elements.appended(element)))
            .asInstanceOf[AddResult[A]]
        }
      case it: CTypeDefinition =>
        if (typeDefinitions.contains(it.name)) {
          fail(
            TypeWithNameAlreadyExists(this, it.name, it._type)
          )
            .asInstanceOf[AddResult[A]]
        } else {
          succeed(Context(elements = elements.appended(element)))
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

  def getTypeDefinition(name: String): IO[AppError, Type] =
    elements
      .mapFilter {
        case CTypeDefinition(name1, _type) if name == name1 => Some(_type)
        case _                                              => None
      }
      .headOption
      .fold(fail(TypeNotKnown(this, name)))(succeed(_))

  def hasExistential(name: String): Boolean =
    elements.contains(CExistential(name))

  def hasTypeDefinition(name: String): Boolean =
    typeDefinitions.contains(name)

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
def checkIsWellFormed(context: Context, _type: Type): IO[AppError, Unit] = {
  _type match {
    case _: TLiteral => ZIO.unit
    case TVariable(name) =>
      if (context.hasVariable(name)) ZIO.unit
      else fail(TypeNotWellFormed(context, _type))
    case TLambda(arg, ret) =>
      checkIsWellFormed(context, arg) *> checkIsWellFormed(context, ret)
    case TQuantification(alpha, a) =>
      checkIsWellFormed(context.add(CVariable(alpha)), a)
    case TExistential(name) =>
      if (context.hasExistential(name) || context.getSolved(name).isDefined) {
        ZIO.unit
      } else {
        fail(TypeNotWellFormed(context, _type))
      }
    case TTuple(valueTypes) =>
      ZIO.forall(valueTypes)(checkIsWellFormed(context, _).as(true)).unit
    case TTypeRef(targetType) =>
      if (context.hasTypeDefinition(targetType)) {
        ZIO.unit
      } else {
        fail(TypeNotKnown(context, targetType))
      }
    case TStruct(fieldTypes) =>
      ZIO.forall(fieldTypes.values)(checkIsWellFormed(context, _).as(true)).unit
  }
}

// Fig 8
def applyContext(_type: Type, context: Context): IO[AppError, Type] = {
  _type match {
    case TLiteral(_)  => succeed(_type)
    case TVariable(_) => succeed(_type)
    case TExistential(name) => {
      context.getSolved(name).fold(succeed(_type))(applyContext(_, context))
    }
    case TLambda(argType, returnType) =>
      for {
        arg  <- applyContext(argType, context)
        body <- applyContext(returnType, context)
      } yield TLambda(arg, body)
    case TQuantification(name, quantType) => {
      applyContext(quantType, context).map(TQuantification(name, _))
    }
    case TTuple(valueTypes) =>
      ZIO.foreach(valueTypes)(applyContext(_, context)).map(TTuple(_))
    case TTypeRef(name) =>
      context
        .getTypeDefinition(name)
        .flatMap(applyContext(_, context))
    case TStruct(fieldTypes) =>
      ZIO
        .foreach(fieldTypes) { case (k, v) =>
          applyContext(v, context).map((k, _))
        }
        .map(TStruct.apply)
  }
}
