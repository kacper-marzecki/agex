extension [A](it: Vector[A]) {
  def findIndexOf(elem: A) =
    it.indexOf(it) match {
      case -1    => None
      case index => Some(index)
    }
}
