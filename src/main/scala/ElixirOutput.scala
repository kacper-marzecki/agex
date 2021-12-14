import TypedModule.*
import TypedStatement.*
import TypedExpression.*
import TypedPattern.*
import Literal.*
object ElixirOutput {
  def toElixir(it: Literal): String = {
    it match {
      case LChar(it: Char)     => it.toString
      case LString(it: String) => it
      case LInt(it: Int)       => it.toString
      case LFloat(it: Float)   => it.toString
      case LBool(it: Boolean)  => it.toString
      case LAtom(it: String)   => s":$it"
      case LNil                => "nil"
      case LUnit               => ""
    }
  }

  def toElixir(expr: TypedExpression): String = {
    expr match {
      case TEVariable(name, _type) => name
      case TELiteral(it, _type)    => toElixir(it)
      // Why does it exist ?
      case TEAny(expression: TypedExpression, _type: Type) =>
        toElixir(expression)
      case TELet(name, value, body, _type) =>
        s"""
        $name = ${toElixir(value)}
        ${toElixir(body)}
        """
      case TEAnnotation(expr, annotatedType, _type) => toElixir(expr)
      case TETuple(values, _type) =>
        "{" + values.map(toElixir).mkString(", ") + "}"
      case TETypeAlias(newName, targetType, expr, _type) => ""
      case TEStruct(fields: Map[String, TypedExpression], _type) =>
        ???
      case TEList(values, _type) =>
        "[" + values.map(toElixir).mkString(", ") + "]"
      case TEMap(kvs: List[(TypedExpression, TypedExpression)], _type: Type) =>
        "%{" + kvs
          .map { case (k, v) => s"${toElixir(k)} => ${toElixir(v)}" }
          .mkString(", \n") + "}"
      case TEFunction(args: List[String], body: TypedExpression, _type: Type) =>
        val argumentList = if (args.isEmpty) "" else s"(${args.mkString(", ")})"
        s"fn ${argumentList} -> ${toElixir(body)} end"
      case TEFunctionApplication(fun, args, _type) =>
        s"apply(${toElixir(fun)}, [${args.map(toElixir).mkString(", ")}])"
      case TEIf(condition, ifTrue, ifFalse, _type) =>
        s"""if ${toElixir(condition)} do
            ${toElixir(ifTrue)} 
          else 
            ${toElixir(ifFalse)} 
          end 
        """
      case TECase(expr, cases, _type) =>
        val caseLines = cases
          .map { case (pattern, e) =>
            s"${toElixir(pattern)} -> ${toElixir(e)}"
          }
          .mkString("\n")
        s"""
         case ${toElixir(expr)} do 
          ${caseLines}
         end
        """
    }
  }

  def toElixir(it: TypedPattern): String = it match {
    case TPPin(expression)  => "^" + toElixir(expression)
    case TPVar(name)        => name
    case TPLiteral(literal) => toElixir(literal.it)
    case TPList(values) =>
      s"[${values.map(toElixir).mkString(", ")}]"
    // TODO fucked up with this representation of rest
    case TPListRest => ???
    case TPTuple(values) =>
      s"{${values.map(toElixir).mkString(", ")}}"
    case TPMap(kvs: List[(TypedPattern, TypedPattern)]) =>
      "%{" + kvs
        .map { case (k, v) => s"${toElixir(k)} => ${toElixir(v)}" }
        .mkString(", ") + "}"

  }

  def toElixir(it: FunctionDef): String = {
    s"""
    def ${it.name}(${it.args.mkString(", ")}) do 
      ${toElixir(it.body)}
    end 
    """
  }
  def toElixir(it: ModuleAttribute): String = {
    s"@${it.name} ${toElixir(it.body)}"
  }

  def toElixir(module: TypedModule): String = {
    val aliases = module.aliases.map(a => s"alias $a\n")
    val statements = module.statements.map {
      case it: FunctionDef     => toElixir(it)
      case it: ModuleAttribute => toElixir(it)
    }
    s"""
    defmodule ${module.name} do
    $aliases

    end
    """
  }
}
