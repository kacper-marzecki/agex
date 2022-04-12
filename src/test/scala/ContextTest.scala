import zio._
import zio.console._
import zio.test._
import zio.test.Assertion._
import zio.test.environment._
import Type._
import LiteralType._
import ContextElement._

object ContextTest extends DefaultRunnableSpec {
  val solved1 = CSolved("solved1", TLiteral(LTChar))
  val solved2 = CSolved("solved2", TLiteral(LTBool))
  val solved3 = CSolved("solved3", TLiteral(LTFloat))
  val solved4 = CSolved("solved4", TLiteral(LTInt))
  val solved5 = CSolved("solved5", TLiteral(LTString))
  val context = Context(Vector(solved1, solved2, solved3))

  def spec = suite("ContextTest")(
    testM("split works") {
      for {
        result <- context.splitAt(solved2)
        (one, two) = result
      } yield assert(one.elements)(equalTo(Vector(solved1))) &&
        assert(two.elements)(equalTo(Vector(solved2, solved3)))
    },
    testM("insertInPlace works") {
      context
        .insertInPlace(solved2, List(solved4, solved5))
        .map(result =>
          assert(result.elements)(
            equalTo(Vector(solved1, solved4, solved5, solved3))
          )
        )
    },
    testM("drop works") {
      assertM(context.drop(solved2).map(_.elements))(equalTo(Vector(solved1)))
    },
    test("getSolved works") {
      assert(context.getSolved(solved3.name))(equalTo(Some(solved3._type)))
    }
    // TODO: failing due to temp turn-off of type variable shadow check
    // testM("doesnt allow duplicate type variable names") {
    //   val variable = CTypedVariable("a", TLiteral(LTInt))
    //   val ctx      = Context(Vector(variable))
    //   assertM(ctx.add(variable).flip)(
    //     equalTo(
    //       AppError.ShadowedVariableName(ctx, variable.name)
    //     )
    //   )
    // },
    // testM("doesnt allow duplicate type definition names") {
    //   val typeDef = CTypeDefinition("a", TLiteral(LTInt))
    //   val ctx     = Context(Vector(typeDef))
    //   assertM(ctx.add(typeDef).flip)(
    //     equalTo(
    //       AppError.TypeWithNameAlreadyExists(ctx, typeDef.name, typeDef._type)
    //     )
    //   )
    // }
  )
}
