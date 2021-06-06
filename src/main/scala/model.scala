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
  case Literal(literalType: LiteralType)
  case Variable(name: String)
  case Existential(name: String)
  case Quantification(name: String, _type: Type)
  case Function(arg: Type, ret: Type)
  case Product(one: Type, two: Type)
}

extension (t: Type) {
  def isMonotype: Boolean =
    t match {
      case _: Type.Quantification  => false
      case Type.Function(arg, ret) => arg.isMonotype && ret.isMonotype
      case _                       => true
    }
}

enum LiteralType {
  case LTChar
  case LTUnit
  case LTString
  case LTInt
  case LTFloat
  case LTBool
}

enum ContextElement {
  case Variable(name: String)
  case Existential(name: String)
  case Solved(name: String, _type: Type)
  case Marker(name: String)
  case TypedVariable(name: String, _type: Type)
}
