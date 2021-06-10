enum Literal {
  case LChar(it: Char)
  case LString(it: String)
  case LInt(it: Int)
  case LFloat(it: Float)
  case LBool(it: Boolean)
  case LUnit
}

enum TypedExpression(val _type: Type) {
  case TEVariable(name: String, override val _type: Type)
      extends TypedExpression(_type)
  case TELiteral(it: Literal, override val _type: Type)
      extends TypedExpression(_type)
  case TELambda(
      arg: String,
      body: TypedExpression,
      override val _type: Type
  ) extends TypedExpression(_type)
  case TEApplication(
      fun: TypedExpression,
      arg: TypedExpression,
      override val _type: Type
  ) extends TypedExpression(_type)
  case TELet(
      name: String,
      value: TypedExpression,
      body: TypedExpression,
      override val _type: Type
  ) extends TypedExpression(_type)
  case TEAnnotation(
      expr: TypedExpression,
      annotatedType: Type,
      override val _type: Type
  ) extends TypedExpression(_type)
  case TETuple(
      one: TypedExpression,
      two: TypedExpression,
      override val _type: Type
  ) extends TypedExpression(_type)
}

enum Expression {
  case EVariable(name: String)
  case ELiteral(it: Literal)
  case ELambda(arg: String, body: Expression)
  case EApplication(fun: Expression, arg: Expression)
  case ELet(name: String, value: Expression, body: Expression)
  case EAnnotation(expr: Expression, annotatedType: Type)
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

sealed trait ContextElement(val name: String)
case class CVariable(override val name: String) extends ContextElement(name)
case class CExistential(override val name: String) extends ContextElement(name)
case class CSolved(override val name: String, _type: Type)
    extends ContextElement(name)
case class CTypedVariable(override val name: String, _type: Type)
    extends ContextElement(name)
case class CMarker(override val name: String) extends ContextElement(name)
