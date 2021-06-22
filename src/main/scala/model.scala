sealed trait Literal
object Literal {
  case class LChar(it: Char)     extends Literal
  case class LString(it: String) extends Literal
  case class LInt(it: Int)       extends Literal
  case class LFloat(it: Float)   extends Literal
  case class LBool(it: Boolean)  extends Literal
  case class LAtom(it: String)   extends Literal
  case object LNil               extends Literal
  case object LUnit              extends Literal
}

/** When adding a new expression
  *   - update synthesis logic
  *   - update checksAgainst
  */
sealed trait Expression
object Expression {
  case class EVariable(name: String) extends Expression
  case class ELiteral(it: Literal)   extends Expression
  case class ELet(name: String, value: Expression, body: Expression)
      extends Expression
  case class EAnnotation(expr: Expression, annotatedType: Type)
      extends Expression
  case class ETuple(values: List[Expression]) extends Expression
  case class ETypeAlias(newName: String, targetType: Type, expr: Expression)
      extends Expression
  case class EStruct(fields: Map[String, Expression])        extends Expression
  case class EFunction(args: List[String], body: Expression) extends Expression
  case class EFunctionApplication(fun: Expression, args: List[Expression])
      extends Expression
  case class EIf(condition: Expression, ifTrue: Expression, ifFalse: Expression)
      extends Expression
}

sealed trait LiteralType
object LiteralType {
  case object LTChar   extends LiteralType
  case object LTUnit   extends LiteralType
  case object LTString extends LiteralType
  case object LTInt    extends LiteralType
  case object LTFloat  extends LiteralType
  case object LTAtom   extends LiteralType
  case object LTBool   extends LiteralType
  case object LTNil    extends LiteralType
}

/** When adding a new Type:
  *   - update instantiation logic
  *   - update checksAgainst logic
  *   - update subtyping logic
  */
sealed trait Type {
  def isMonotype: Boolean =
    this match {
      case _: Type.TQuantification => false
      case Type.TFunction(args, ret) =>
        args.forall(_.isMonotype) && ret.isMonotype
      case _ => true
    }
}
object Type {
  case class TLiteral(literalType: LiteralType)         extends Type
  case class TVariable(name: String)                    extends Type
  case class TExistential(name: String)                 extends Type
  case class TQuantification(name: String, _type: Type) extends Type
  case class TTuple(valueTypes: List[Type])             extends Type
  case class TTypeRef(targetType: String)               extends Type
  case class TStruct(fieldTypes: Map[String, Type])     extends Type
  case class TFunction(args: List[Type], ret: Type)     extends Type
}

sealed trait TypedExpression {
  def _type: Type
}
object TypedExpression {
  case class TEVariable(name: String, _type: Type) extends TypedExpression
  case class TELiteral(it: Literal, _type: Type)   extends TypedExpression
  case class TELet(
      name: String,
      value: TypedExpression,
      body: TypedExpression,
      _type: Type
  ) extends TypedExpression
  case class TEAnnotation(
      expr: TypedExpression,
      annotatedType: Type,
      _type: Type
  ) extends TypedExpression
  case class TETuple(
      values: List[TypedExpression],
      _type: Type
  ) extends TypedExpression
  case class TETypeAlias(
      newName: String,
      targetType: Type,
      expr: TypedExpression,
      _type: Type
  ) extends TypedExpression
  case class TEStruct(
      fields: Map[String, TypedExpression],
      _type: Type
  ) extends TypedExpression
  case class TEFunction(args: List[String], body: TypedExpression, _type: Type)
      extends TypedExpression
  case class TEFunctionApplication(
      fun: TypedExpression,
      args: List[TypedExpression],
      _type: Type
  ) extends TypedExpression
  case class TEIf(
      condition: TypedExpression,
      ifTrue: TypedExpression,
      ifFalse: TypedExpression,
      _type: Type
  ) extends TypedExpression
}

sealed trait ContextElement {
  def name: String
}
object ContextElement {
  case class CVariable(name: String)                    extends ContextElement
  case class CExistential(name: String)                 extends ContextElement
  case class CSolved(name: String, _type: Type)         extends ContextElement
  case class CTypedVariable(name: String, _type: Type)  extends ContextElement
  case class CMarker(name: String)                      extends ContextElement
  case class CTypeDefinition(name: String, _type: Type) extends ContextElement
}
