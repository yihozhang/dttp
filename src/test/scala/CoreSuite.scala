import org.scalatest.FunSuite
import Core._
import Gamma._

class CoreSuite extends FunSuite {
    test("basic programs") {
        val program = App(Abs("x", Nat, Add1(Var("x"))), Zero).toValue
        assertResult(Add1(Zero).toValue) {
            program
        }
        assertResult(None) {
            Nat.toValue.describes(program)
        }
        // assertResult(None) {
        //     Π("x", Nat, Nat, Gamma.initial).toValue describes Abs("x", Nat, Add1(Var("x"))).toValue
        // }
        println(Abs("x", Nat, Add1(Var("x"))).toValue.synth)
    }

    // test("Pi type") {
    //     val program: Core = 
    // }
}