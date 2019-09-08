import org.scalatest.FunSuite
import Src._
import Gamma._

class CoreSuite extends FunSuite {
    test("basic programs") {
        // val program: Core = App(Abs("x", Nat, Add1(Var("x"))), Zero)
        // assertResult(Add1(Var("x"))) {
        //     program.toValue(Gamma.initial)._1
        // }
        // assertResult(None) {
        //     Nat.describes(program)(Gamma.initial)
        // }
        // assertResult(None) {
        //     implicit val initial = Gamma.initial
        //     Arrow(Nat, Nat).toCore.describes(Abs("x", Nat, Add1(Var("x"))))
        // }
        // TODO: create a syntax form of program
    }

    // test("Pi type") {
    //     val program: Core = 
    // }
}