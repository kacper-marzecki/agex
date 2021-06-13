import zio.*
import zio.console.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.environment.*

object UtilsTest extends DefaultRunnableSpec {

  def spec = suite("Utils test")(
    testM("anyM works 1") {
      val collection = List(1, 2, 3, 4, 5)
      val eff = anyM(
        collection,
        it => if (it == 5) ZIO.succeed(true) else ZIO.succeed(true)
      )
      assertM(eff)(isTrue)
    },
    testM("anyM works 2") {
      val collection = List(1, 2, 3, 4)
      val eff = anyM(
        collection,
        it => if (it == 5) ZIO.succeed(true) else ZIO.succeed(false)
      )
      assertM(eff)(isFalse)
    },
    testM("anyM works 3") {
      val collection = List(1, 2, 3, 4, 5)
      val eff = anyM(
        collection,
        it => if (it == 5) ZIO.fail("SomeError") else ZIO.succeed(false)
      )
      assertM(eff.flip)(equalTo("SomeError"))
    }
  )
}
