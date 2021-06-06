import cats.implicits.*
case class Context(elements: Vector[ContextElement] = Vector.empty) {
  def add(it: ContextElement) =
    this.copy(elements = elements.appended(it))

  // TODO throws
  def splitAt(it: ContextElement): (Context, Context) = {
    elements.findIndexOf(it) match {
      case None => ???
      case Some(index) =>
        val (l, r) = elements.splitAt(index)
        (Context(l), Context(r))
    }
  }

  def insertInPlace(
      element: ContextElement,
      inserts: List[ContextElement]
  ): Context = {
    val newElements = elements.findIndexOf(element) match {
      case None        => ???
      case Some(index) => elements.patch(index, inserts, 1)
    }
    Context(newElements)
  }

  def drop(element: ContextElement): Context = {
    elements.findIndexOf(element) match {
      case None => ???
      // TODO possible bug: check how splitAt splits the coll
      case Some(index) => Context(elements.splitAt(index)._1)
    }
  }

  def getSolved(name: String): Option[Type] =
    elements.mapFilter {
      case ContextElement.Solved(name1, _type) if name == name1 => Some(_type)
      case _                                                    => None
    }.headOption

  def hasExistential(name: String): Boolean =
    elements.contains(ContextElement.Existential(name))

  def hasVariable(name: String): Boolean =
    elements.contains(ContextElement.Variable(name))

  def getAnnotation(name: String): Option[Type] =
    elements.mapFilter {
      case ContextElement.TypedVariable(name1, _type) if name == name1 =>
        Some(_type)
      case _ => None
    }.headOption
}
