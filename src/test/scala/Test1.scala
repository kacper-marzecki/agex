import org.junit.Test
import org.junit.Assert.*

class Test1:
  @Test def t1(): Unit =
    assertEquals("I was compiled by Scala 3. :)", msg)

import zio._
import zio.console._
import zio.test._
import zio.test.Assertion._
import zio.test.environment._
import java.io.IOException

def solved(name: String, _type: Type): ContextElement.Solved =
  ContextElement.Solved(name, _type)

object ContextTest extends DefaultRunnableSpec {
  val solved1 = solved("solved1", Type.Literal(LiteralType.LTChar))
  val solved2 = solved("solved2", Type.Literal(LiteralType.LTBool))
  val solved3 = solved("solved3", Type.Literal(LiteralType.LTFloat))
  val solved4 = solved("solved4", Type.Literal(LiteralType.LTInt))
  val solved5 = solved("solved5", Type.Literal(LiteralType.LTString))
  val context = Context(Vector(solved1, solved2, solved3))

  def spec = suite("ContextSpec")(
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
  )
}
