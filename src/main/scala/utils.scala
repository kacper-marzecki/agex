extension [A](it: Vector[A]) {
  def findIndexOf(elem: A) =
    it.indexWhere(_ == elem) match {
      case -1    => None
      case index => Some(index)
    }
}
