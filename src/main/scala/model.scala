enum Literal {
  case LChar(it: Char)
  case LString(it: String)
  case LInt(it: Int)
  case LFloat(it: Float)
  case LBool(it: Boolean)
  case LUnit
}

enum Expression {
  case EVariable(name: String)
  case ELiteral(it: Literal)
  case EAbstraction(arg: String, body: Expression)
  case EApplication(fun: Expression, arg: Expression)
  case ELet(name: String, value: Expression, body: Expression)
  case EAnnotation(expr: Expression, _type: Type)
  case ETuple(one: Expression, two: Expression)
}

enum Type {
  def isMonotype: Boolean =
    this match {
      case _: Type.TQuantification  => false
      case Type.TFunction(arg, ret) => arg.isMonotype && ret.isMonotype
      case _                        => true
    }
  case TLiteral(literalType: LiteralType)
  case TVariable(name: String)
  case TExistential(name: String)
  case TQuantification(name: String, _type: Type)
  case TFunction(arg: Type, ret: Type)
  case TProduct(one: Type, two: Type)
}

enum LiteralType {
  case LTChar
  case LTUnit
  case LTString
  case LTInt
  case LTFloat
  case LTBool
}

enum ContextElement(val name: String) {
  case Variable(override val name: String) extends ContextElement(name)
  case Existential(override val name: String) extends ContextElement(name)
  case Solved(override val name: String, _type: Type)
      extends ContextElement(name)
  case TypedVariable(override val name: String, _type: Type)
      extends ContextElement(name)
  case Marker(override val name: String) extends ContextElement(name)
}
