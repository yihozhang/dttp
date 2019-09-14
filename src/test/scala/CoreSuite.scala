import org.scalatest.FunSuite
import Data.Src._
import Op.Infer._
import Data.Gamma._
import Data.Value
import Utils.Conversion._
class CoreSuite extends FunSuite {
    test("basic programs") {
        val loc = new Loc()
        implicit val Γ = Gamma.initial
        implicit val ρ = new Env(Nil)
        val fun = Abs(loc, "x", Add1(loc, Var(loc, "x")))
        val natnat = →(loc, Nat(loc), Nat(loc))
        val vnatnat = Op.Check.check(natnat, Value.U).get
        println(vnatnat)
        val zero = Zero(loc)
        val app = App(loc, fun, zero)
        println(Op.Check.check(fun, vnatnat.toValue))
        // println(Op.Check.check(app, Value.Nat))
        // assertResult(None) {
            // Nat.toValue.describes(program)
        // }
        // assertResult(None) {
        //     Π("x", Nat, Nat, Gamma.initial).toValue describes Abs("x", Nat, Add1(Var("x"))).toValue
        // }
        // println(Abs("x", Nat, Add1(Var("x"))).toValue.synth)
    }

    // test("Pi type") {
    //     val program: Core = 
    // }
}