import zio.test.*
import zio.test.Assertion.*
import zio.test.environment.*
import Type.*
import LiteralType.*
import ContextElement.*
import cats.data.NonEmptyList

object ParserTest extends DefaultRunnableSpec {
  def success[A](it: A) = Right(("", it))

  def spec = suite("ParserTest")(
    test("string test") {
      assert(Parser.pString.parse(""""as\" d """"))(
        equalTo(success(SString("as\" d ")))
      )
    },
    test("other test") {
      assert(Parser.pIdentifier.parse("""  asd """))(
        equalTo(success(SId("asd")))
      )
    },
    test("Float test ") {
      assert(Parser.pFloat.parseAll("""  42.1           """))(
        equalTo(Right(SFloat("42.1")))
      )
    },
    test("Sexp test") {
      assert(Parser.pExpr.parseAll("""(+ a b)"""))(
        equalTo(Right(SExp(List(SId("+"), SId("a"), SId("b")))))
      )
    },
    test("Sexp test 2") {
      assert(Parser.pExpr.parseAll("""((a) )"""))(
        equalTo(Right(SExp(List(SExp(List(SId("a")))))))
      )
    },
    test("Sexp test 3") {
      assert(
        Parser.pExpr.parseAll(
          """     (  if (   = someVariable 42      41.1      ) (do-something       :ok) (   raise-error :error))     """
        )
      )(
        equalTo(
          Right(
            SExp(
              List(
                SId("if"),
                SExp(
                  List(
                    SId("="),
                    SId("someVariable"),
                    SInt("42"),
                    SFloat("41.1")
                  )
                ),
                SExp(List(SId("do-something"), SId(":ok"))),
                SExp(List(SId("raise-error"), SId(":error")))
              )
            )
          )
        )
      )
    },
    test("Sexp test 3") {
      assert(
        Parser.pExpr.parseAll(
          """(if (= someVariable 42 41.1) 
             |  (do-something :ok)
             |  (raise-error :error))""".stripMargin
        )
      )(
        equalTo(
          Right(
            SExp(
              List(
                SId("if"),
                SExp(
                  List(
                    SId("="),
                    SId("someVariable"),
                    SInt("42"),
                    SFloat("41.1")
                  )
                ),
                SExp(List(SId("do-something"), SId(":ok"))),
                SExp(List(SId("raise-error"), SId(":error")))
              )
            )
          )
        )
      )
    }
  )
}
