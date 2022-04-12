import zio.test._
import zio.test.Assertion._
import zio.test.environment._
import Type._
import LiteralType._
import ContextElement._
import cats.data.NonEmptyList

object TokenizerTest extends DefaultRunnableSpec {
  def eq(it: SExp): Assertion[Either[Any, SExp]] = equalTo(Right(it))

  def spec = suite("TokenizerTest")(
    test("string test") {
      assert(Tokenizer.pExpr.parseAll(""" "asd \" a" """))(
        eq(SString("asd \" a"))
      )
    },
    test("other test") {
      assert(Tokenizer.pIdentifier.parseAll("""  asd """))(
        eq(SId("asd"))
      )
    },
    test("Float test ") {
      assert(Tokenizer.pIdentifier.parseAll("""  42.1           """))(
        eq(SId("42.1"))
      )
    },
    test("Sexp test") {
      assert(Tokenizer.pExpr.parseAll("""(+ a b)"""))(
        eq(SList(List(SId("+"), SId("a"), SId("b"))))
      )
    },
    test("Sexp test 2") {
      assert(Tokenizer.pExpr.parseAll("""((a) )"""))(
        eq(SList(List(SList(List(SId("a"))))))
      )
    },
    test("Sexp test 3") {
      assert(
        Tokenizer.pExpr.parseAll(
          """     (  if (   = someVariable 42      41.1      ) (do-something       :ok) (   raise-error :error))     """
        )
      )(
        eq(
          SList(
            List(
              SId("if"),
              SList(
                List(
                  SId("="),
                  SId("someVariable"),
                  SId("42"),
                  SId("41.1")
                )
              ),
              SList(List(SId("do-something"), SId(":ok"))),
              SList(List(SId("raise-error"), SId(":error")))
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
          SList(
            List(
              SId("if"),
              SList(
                List(
                  SId("="),
                  SId("someVariable"),
                  SId("42"),
                  SId("41.1")
                )
              ),
              SList(List(SId("do-something"), SId(":ok"))),
              SList(List(SId("raise-error"), SId(":error")))
            )
          )
        )
      )
    },
    test("Brace kinds") {
      assert(
        Tokenizer.pExpr.parseAll(
          """(fn [acc elem] 
             |  (push acc {:result (extract-result elem)}))""".stripMargin
        )
      )(
        eq(
          SList(
            List(
              SId("fn"),
              SSquareList(List(SId("acc"), SId("elem"))),
              SList(
                List(
                  SId("push"),
                  SId("acc"),
                  SCurlyList(
                    List(
                      SId(":result"),
                      SList(List(SId("extract-result"), SId("elem")))
                    )
                  )
                )
              )
            )
          )
        )
      )
    }
  )
}
