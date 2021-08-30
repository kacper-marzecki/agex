import Type.TMap
import Expression.*

import zio.ZIO
import TMapping.Optional
import TMapping.Required
import Type.*
import TypedExpression.TEMap
def checksAgainstMap(
    context: Context,
    expr: EMap,
    mapType: TMap
): Eff[(TypedExpression, Context)] = {
  ZIO.foldLeft(mapType.kvs)(Nil) {
    case (agg, TMapping.Required(k, v)) => ???
    case (agg, TMapping.Optional(k, v)) => ???
  }
  // foreach key-value binding
  //// check if key is a sum type

  //// check if map value contains a key of this type
  //// ^ validate ALL value-keys of the type k have value of the type v (there can be multiple keys of the type) (works only for non-sum types ? )
  ???
}

def mapSynthesizesTo(
    context: Context,
    emap: EMap
): Eff[(TypedExpression, Context)] = {
  for {
    (kvs, mappings, ctx) <- ZIO.foldRight(emap.kvs)(
      (List[(TypedExpression, TypedExpression)](), List[TMapping](), context)
    ) { case ((k, v), (typedKvs, typeMappings, ctx)) =>
      for {
        (kTyped, gamma) <- synthesizesTo(ctx, k)
        mapping =
          if (shouldHaveOptionalMapping(kTyped._type)) Optional.apply
          else Required.apply
        (vTyped, delta) <- synthesizesTo(gamma, v)
      } yield (
        (kTyped, vTyped) :: typedKvs,
        mapping(kTyped._type, vTyped._type) :: typeMappings,
        delta
      )
    }
  } yield (TEMap(kvs, TMap(mappings)), ctx)
}

def shouldHaveOptionalMapping(_type: Type): Boolean = {
  val containsOptionalType = (types: Iterable[Type]) =>
    types.find(shouldHaveOptionalMapping).isDefined

  _type match {
    case TLiteral(literalType)        => false
    case it: TValue                   => false
    case TList(valueType)             => shouldHaveOptionalMapping(valueType)
    case TAny                         => true
    case TNothing                     => false
    case TVariable(name)              => true
    case TExistential(name)           => true
    case it: TMulQuantification       => true
    case it: TTypeApp                 => true
    case TQuantification(name, _type) => true
    case TTuple(valueTypes)           => containsOptionalType(valueTypes)
    // should fetch the type from context
    case TTypeRef(targetType) => ???
    case TStruct(fieldTypes)  => containsOptionalType(fieldTypes.values.toList)
    case TMap(kvs) => containsOptionalType(kvs.flatMap(it => List(it.k, it.v)))
    case TFunction(args, ret) => false
    case TSum(a, b)           => true
  }
}
