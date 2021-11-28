import cats.implicits.*
import zio.*
import ZIO.{fail, succeed}
import AppError.*
import ContextElement.*
import Type.*
import TMapping.*
import cats.syntax.apply
import scala.jdk.FunctionWrappers.RichToLongFunctionAsFunction1

type AddError  = ShadowedVariableName | TypeWithNameAlreadyExists
type AddResult = IO[AddError, Context]

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

  def add(element: ContextElement): AddResult =
    // TODO: ACHTUNG, may break many things
    // Temporarly turned-off type variable shadowing checks
    // validateCanAdd(element)
    ZIO.unit
      .as(Context(elements = elements.appended(element)))

  def addAll(newElements: Iterable[ContextElement]): AddResult =
    ZIO.foldLeft(newElements)(this) { case (delta, elem) => delta.add(elem) }

  def addAll(newElements: ContextElement*): AddResult =
    addAll(newElements)

  private def validateCanAdd(element: ContextElement): IO[AddError, Unit] =
    element match {
      case _: CTypedVariable =>
        if (typedVariableNames.contains(element.name))
          fail(ShadowedVariableName(this, element.name))
        else ZIO.unit
      case it: CTypeDefinition =>
        if (typeDefinitions.contains(it.name))
          fail(TypeWithNameAlreadyExists(this, it.name, it._type))
        else ZIO.unit
      case _ => ZIO.unit
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

  /** As the Context is ordered, it will assume that the List is in the same
    * order, and remove the head of the list from the context
    *
    * TODO: improve this API, as it requires this ^ implicit contract
    */
  def drop(elements: List[ContextElement]): IO[ElementNotFound, Context] =
    ZIO.foldRight(elements)(this)((a, acc) => acc.drop(a))
  // elements.headOption.map(drop(_)).getOrElse(succeed(this))

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
      case CTypeDefinition(name1, _type) if name == name1 =>
        Some(_type)
      case _ => None
    }.headOption
}

/// Fig 7
def checkIsWellFormed(context: Context, _type: Type): IO[AppError, Unit] = {
  _type match {
    case _: TLiteral      => ZIO.unit
    case _: TValue        => ZIO.unit
    case TAny             => ZIO.unit
    case TNothing         => ZIO.unit
    case TList(valueType) => checkIsWellFormed(context, valueType)
    case TVariable(name) =>
      if (context.hasTypeDefinition(name) || context.hasVariable(name)) ZIO.unit
      else
        fail(TypeNotWellFormed(context, _type))
    case TFunction(args, ret) =>
      ZIO.foreach_(ret :: args)(checkIsWellFormed(context, _))
    case TQuantification(alpha, a) =>
      context
        .add(CVariable(alpha))
        .flatMap(checkIsWellFormed(_, a))
    case it: TMulQuantification =>
      context
        .addAll(it.names.map(CVariable(_)))
        .flatMap(checkIsWellFormed(_, it._type))
    case TExistential(name) =>
      if (context.hasExistential(name) || context.getSolved(name).isDefined) {
        ZIO.unit
      } else {
        fail(TypeNotWellFormed(context, _type))
      }
    case TTuple(valueTypes) =>
      ZIO.foreach_(valueTypes)(checkIsWellFormed(context, _))
    case TTypeRef(targetType) =>
      if (context.hasTypeDefinition(targetType)) {
        ZIO.unit
      } else {
        fail(TypeNotKnown(context, targetType))
      }
    case TSum(x, y) => ZIO.foreach_(List(x, y))(checkIsWellFormed(context, _))
    case it @ TTypeApp(_type, args) =>
      ZIO.foreach_(_type :: args)(
        checkIsWellFormed(context, _)
      ) *> it
        .applyType(context)
        .unit
    case TStruct(fieldTypes) =>
      ZIO.foreach_(fieldTypes.values)(checkIsWellFormed(context, _))
    case TMap(mappings) =>
      ZIO.foreach_(mappings.flatMap(it => List(it.k, it.v)))(
        checkIsWellFormed(context, _)
      )
  }
}

// Fig 8
def applyContext(_type: Type, context: Context): IO[AppError, Type] = {
  _type match {
    case TValue(_)        => succeed(_type)
    case TLiteral(_)      => succeed(_type)
    case TVariable(_)     => succeed(_type)
    case TAny             => succeed(_type)
    case TNothing         => succeed(_type)
    case TList(valueType) => applyContext(valueType, context).map(TList(_))
    case TExistential(name) => {
      context.getSolved(name).fold(succeed(_type))(applyContext(_, context))
    }
    case TFunction(argTypes, returnType) =>
      for {
        args <- ZIO.foreach(argTypes)(applyContext(_, context))
        body <- applyContext(returnType, context)
      } yield TFunction(args, body)
    case TQuantification(name, quantType) => {
      applyContext(quantType, context).map(TQuantification(name, _))
    }
    case TMulQuantification(names, quantType) =>
      // 1:1 TQuantification port
      applyContext(quantType, context).map(TMulQuantification(names, _))
    case TSum(x, y) =>
      for {
        appliedX <- applyContext(x, context)
        appliedY <- applyContext(y, context)
      } yield TSum(appliedX, appliedY)
    case TTypeApp(quant, args) =>
      for {
        q <- applyContext(quant, context)
        a <- ZIO.foreach(args)(applyContext(_, context))
      } yield (TTypeApp(q, a))
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
    case TMap(mappings) =>
      ZIO
        .foreach(mappings) { it =>
          it match {
            case Required(k, v) =>
              for {
                appliedK <- applyContext(k, context)
                appliedV <- applyContext(v, context)
              } yield Required(appliedK, appliedV)
            case Optional(k, v) =>
              for {
                appliedK <- applyContext(k, context)
                appliedV <- applyContext(v, context)
              } yield Optional(appliedK, appliedV)
          }

        }
        .map(TMap(_))
  }
}
