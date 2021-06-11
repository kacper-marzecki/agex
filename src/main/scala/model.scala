sealed trait Literal
object Literal {
  case class LChar(it: Char) extends Literal
  case class LString(it: String) extends Literal
  case class LInt(it: Int) extends Literal
  case class LFloat(it: Float) extends Literal
  case class LBool(it: Boolean) extends Literal
  case object LUnit extends Literal
}

sealed trait TypedExpression(val _type: Type)
object TypedExpression {
  case class TEVariable(name: String, override val _type: Type)
      extends TypedExpression(_type)
  case class TELiteral(it: Literal, override val _type: Type)
      extends TypedExpression(_type)
  case class TELambda(
      arg: String,
      body: TypedExpression,
      override val _type: Type
  ) extends TypedExpression(_type)
  case class TEApplication(
      fun: TypedExpression,
      arg: TypedExpression,
      override val _type: Type
  ) extends TypedExpression(_type)
  case class TELet(
      name: String,
      value: TypedExpression,
      body: TypedExpression,
      override val _type: Type
  ) extends TypedExpression(_type)
  case class TEAnnotation(
      expr: TypedExpression,
      annotatedType: Type,
      override val _type: Type
  ) extends TypedExpression(_type)
  case class TETuple(
      values: List[TypedExpression],
      override val _type: Type
  ) extends TypedExpression(_type)
}

sealed trait Expression
object Expression {
  case class EVariable(name: String) extends Expression
  case class ELiteral(it: Literal) extends Expression
  case class ELambda(arg: String, body: Expression) extends Expression
  case class EApplication(fun: Expression, arg: Expression) extends Expression
  case class ELet(name: String, value: Expression, body: Expression)
      extends Expression
  case class EAnnotation(expr: Expression, annotatedType: Type)
      extends Expression
  case class ETuple(values: List[Expression]) extends Expression
}

sealed trait Type {
  def isMonotype: Boolean =
    this match {
      case _: Type.TQuantification  => false
      case Type.TFunction(arg, ret) => arg.isMonotype && ret.isMonotype
      case _                        => true
    }
}
object Type {
  case class TLiteral(literalType: LiteralType) extends Type
  case class TVariable(name: String) extends Type
  case class TExistential(name: String) extends Type
  case class TQuantification(name: String, _type: Type) extends Type
  case class TFunction(arg: Type, ret: Type) extends Type
  case class TTuple(valueTypes: List[Type]) extends Type
}

sealed trait LiteralType
object LiteralType {
  case object LTChar extends LiteralType
  case object LTUnit extends LiteralType
  case object LTString extends LiteralType
  case object LTInt extends LiteralType
  case object LTFloat extends LiteralType
  case object LTBool extends LiteralType
}

sealed trait ContextElement(val name: String)
object ContextElement {
  case class CVariable(override val name: String) extends ContextElement(name)
  case class CExistential(override val name: String)
      extends ContextElement(name)
  case class CSolved(override val name: String, _type: Type)
      extends ContextElement(name)
  case class CTypedVariable(override val name: String, _type: Type)
      extends ContextElement(name)
  case class CMarker(override val name: String) extends ContextElement(name)
}
