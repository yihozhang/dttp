import org.scalatest.FunSuite
import Src._
import Ctx._

class CoreSuite extends FunSuite {
    test("basic programs") {
        val program: Core = App(Abs("x", Nat, Add1(Var("x"))), Zero)
        assertResult(Add1(Var("x"))) {
            program.toValue(new Ctx())._1
        }
        assertResult(None) {
            Nat.describes(program)(new Ctx)
        }
        assertResult(None) {
            implicit val ctx: Ctx = new Ctx
            Arrow(Nat, Nat).toCore.describes(Abs("x", Nat, Add1(Var("x"))))
        }
    }

    // test("Pi type") {
    //     val program: Core = 
    // }
}