import Type.TAny
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

sealed trait AgexModule { def name: String }
sealed trait ElixirModuleStatement
case class ElixirModule(
    name: String,
    aliases: List[String],
    members: List[ElixirModuleStatement]
) extends AgexModule
case class ElixirFunction(
    name: String,
    _type: Type,
    targetFunction: String
) extends ElixirModuleStatement
case class ElixirTypeDef(name: String, _type: Type)
    extends ElixirModuleStatement

case class ModuleDefinition(
    name: String,
    aliases: List[String],
    members: List[Statement]
) extends AgexModule

sealed trait Statement
object Statement {

  /** (def a (fn [number] number) [a] (+ a 1))
    */
  case class FunctionDef(
      name: String,
      _type: Type,
      args: List[String],
      body: Expression
  )                                                          extends Statement
  case class ModuleAttribute(name: String, body: Expression) extends Statement
  case class TypeDef(name: String, _type: Type)              extends Statement
}

case class TypedModule(
    name: String,
    aliases: List[String],
    statements: List[TypedStatement]
)
sealed trait TypedStatement
object TypedStatement {
  case class FunctionDef(
      name: String,
      args: List[String],
      body: TypedExpression
  ) extends TypedStatement
  case class ModuleAttribute(name: String, body: TypedExpression)
      extends TypedStatement
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
  case class EMap(kvs: List[(Expression, Expression)])       extends Expression
  case class EList(values: List[Expression])                 extends Expression
  case class EFunction(args: List[String], body: Expression) extends Expression
  case class EFunctionApplication(fun: Expression, args: List[Expression])
      extends Expression
  case class EIf(condition: Expression, ifTrue: Expression, ifFalse: Expression)
      extends Expression
}

sealed trait ValueType(val literalType: Type.TLiteral)
object ValueType {
  // TODO: do we need a unit and nil value types ?
  // TODO: tuple, struct value types - do they make sense
  import Type.*
  import LiteralType.*
  case class VTAtom(it: String)   extends ValueType(TLiteral(LTAtom))
  case class VTChar(it: Char)     extends ValueType(TLiteral(LTChar))
  case class VTFloat(it: Float)   extends ValueType(TLiteral(LTFloat))
  case object VTUnit              extends ValueType(TLiteral(LTUnit))
  case class VTString(it: String) extends ValueType(TLiteral(LTString))
  case class VTInt(it: Int)       extends ValueType(TLiteral(LTInt))
  case class VTBool(it: Boolean)  extends ValueType(TLiteral(LTBool))

  case object VTNil extends ValueType(TLiteral(LTNil))
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
  *   - update isMonotype logic
  */
sealed trait Type {
  def isMonotype: Boolean =
    this match {
      case _: Type.TQuantification    => false
      case _: Type.TMulQuantification => false
      case Type.TSum(x, y)            => x.isMonotype && y.isMonotype
      // TODO: is TTypeApp a monotype ? i dont think so, eg the arg can be quantified
      case Type.TTypeApp(q, args) =>
        false // args.forall(_.isMonotype) && q.isMonotype
      case Type.TFunction(args, ret) =>
        args.forall(_.isMonotype) && ret.isMonotype
      case _ => true
    }
}

sealed trait TMapping {
  def k: Type
  def v: Type
}
object TMapping {
  case class Required(k: Type, v: Type) extends TMapping
  case class Optional(k: Type, v: Type) extends TMapping
}

object Type {
  case object TAny                                      extends Type
  case object TNothing                                  extends Type
  case class TValue(valueType: ValueType)               extends Type
  case class TLiteral(literalType: LiteralType)         extends Type
  case class TVariable(name: String)                    extends Type
  case class TExistential(name: String)                 extends Type
  case class TQuantification(name: String, _type: Type) extends Type
  case class TMulQuantification(names: List[String], _type: Type) extends Type {
    def desugar = names.foldRight(_type) { case (universalName, qt) =>
      TQuantification(universalName, qt)
    }
  }
  case class TTuple(valueTypes: List[Type])         extends Type
  case class TTypeRef(targetType: String)           extends Type
  case class TStruct(fieldTypes: Map[String, Type]) extends Type
  // case class TSum(types: Set[Type])                 extends Type
  case class TList(valueType: Type)    extends Type
  case class TMap(kvs: List[TMapping]) extends Type
  case class TSum(a: Type, b: Type)    extends Type

  case class TFunction(args: List[Type], ret: Type) extends Type
  // TODO: think out: only quantifications are polymorphic, what if we could introduce a new Type: TypeApplication ? TQuantification then would be something akin to a Type lambda ?
  // If so, we would have to rewrite instantiation rules to work with several quantificators (not sure how to even start)
  // I get a sense that a type lambda and quantifications are not quite the same <duh>
  // maybe directly a new type : TypeLambda
  case class TTypeApp(_type: Type, args: List[Type]) extends Type {
    def applyType(context: Context) = applyContext(_type, context).flatMap {
      case TMulQuantification(names, _type) =>
        if (names.size != args.size) {
          zio.ZIO.fail(AppError.WrongArity(names.size, args.size))
        } else {
          zio.ZIO.succeed(
            names
              .zip(args)
              .foldLeft(_type) {
                case (quantifiedType, (variableName, typeArg)) =>
                  replaceVariable(variableName, quantifiedType, typeArg)
              }
          )
        }
      // case it: TVariable    => zio.ZIO.succeed(this)
      // case it: TExistential => zio.ZIO.succeed(this)
      case other => zio.ZIO.fail(AppError.CannotApplyType(other))
    }
  }

  def replaceVariable(
      variableName: String,
      in: Type,
      replacement: Type
  ): Type = {
    val repl = replaceVariable(variableName, _, replacement)
    in match {
      case TVariable(name) if (name == variableName) => replacement
      case TTuple(valueTypes)  => TTuple(valueTypes.map(repl))
      case TStruct(fieldTypes) => TStruct(fieldTypes.view.mapValues(repl).toMap)
      case TFunction(args, ret) => TFunction(args.map(repl), repl(ret))
      case TTypeApp(typ, args)  => TTypeApp(repl(typ), args.map(repl))
      case TSum(x, y)           => TSum(repl(x), repl(y))
      case other                => other
    }
  }
}

sealed trait TypedExpression {
  def _type: Type
}
object TypedExpression {
  case class TEVariable(name: String, _type: Type) extends TypedExpression
  case class TELiteral(it: Literal, _type: Type)   extends TypedExpression
  case class TEAny(expression: TypedExpression, _type: Type = TAny)
      extends TypedExpression
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
  case class TEList(values: List[TypedExpression], _type: Type)
      extends TypedExpression
  case class TEMap(kvs: List[(TypedExpression, TypedExpression)], _type: Type)
      extends TypedExpression
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
