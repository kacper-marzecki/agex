import zio.test.*
import zio.test.Assertion.*
import zio.test.environment.*
import Type.*
import LiteralType.*
import ContextElement.*
import cats.data.NonEmptyList

object TokenizerTest extends DefaultRunnableSpec {
  def eq(it: SExp) = equalTo(Right(it))

  def spec = suite("TokenizerTest")(
    test("string test") {
      assert(Tokenizer.pExpr.parseAll("""("as\" d ")"""))(
        eq(SSExprs(List(SSString("as\" d "))))
      )
    },
    test("other test") {
      assert(Tokenizer.pIdentifier.parseAll("""  asd """))(
        eq(SSId("asd"))
      )
    },
    test("Float test ") {
      assert(Tokenizer.pIdentifier.parseAll("""  42.1           """))(
        eq(SSId("42.1"))
      )
    },
    test("Sexp test") {
      assert(Tokenizer.pExpr.parseAll("""(+ a b)"""))(
        eq(SSExprs(List(SSId("+"), SSId("a"), SSId("b"))))
      )
    },
    test("Sexp test 2") {
      assert(Tokenizer.pExpr.parseAll("""((a) )"""))(
        eq(SSExprs(List(SSExprs(List(SSId("a"))))))
      )
    },
    test("Sexp test 3") {
      assert(
        Tokenizer.pExpr.parseAll(
          """     (  if (   = someVariable 42      41.1      ) (do-something       :ok) (   raise-error :error))     """
        )
      )(
        eq(
          SSExprs(
            List(
              SSId("if"),
              SSExprs(
                List(
                  SSId("="),
                  SSId("someVariable"),
                  SSId("42"),
                  SSId("41.1")
                )
              ),
              SSExprs(List(SSId("do-something"), SSId(":ok"))),
              SSExprs(List(SSId("raise-error"), SSId(":error")))
            )
          )
        )
      )
    },
    test("Sexp test 3") {
      assert(
        Tokenizer.pExpr.parseAll(
          """(if (= someVariable 42 41.1) 
             |  (do-something :ok)
             |  (raise-error :error))""".stripMargin
        )
      )(
        eq(
          SSExprs(
            List(
              SSId("if"),
              SSExprs(
                List(
                  SSId("="),
                  SSId("someVariable"),
                  SSId("42"),
                  SSId("41.1")
                )
              ),
              SSExprs(List(SSId("do-something"), SSId(":ok"))),
              SSExprs(List(SSId("raise-error"), SSId(":error")))
            )
          )
        )
      )
    }
  )
}
