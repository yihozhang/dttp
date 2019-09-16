import org.scalatest.FunSuite
import Data.Src._
import Op.Infer._
import Op.Check._
import Data.Gamma._
import Data.Value
import Utils.Conversion._
class CoreSuite extends FunSuite {
    test("basic programs") {
        val loc = new Loc()
        implicit val Γ = Gamma.initial
        implicit val r = Renaming.initial
        implicit val Γr = (Γ, r)
        implicit val ρ = new Env(Nil)
        val fun = Abs(loc, "x", Nat(loc), Same(loc, Add1(loc, Var(loc, "x"))))
        val fun_type = Π(loc, "x", Nat(loc), ≡(loc, Nat(loc), Add1(loc,Var(loc,"x"))))
        val fun_type_v = check(fun_type, Value.U).get
        println(check(fun, fun_type_v.toValue))
        val natnat = →(loc, Nat(loc), Nat(loc))
        val cnatnat = Op.Check.check(natnat, Value.U).get
        val cfun = Op.Check.check(fun, cnatnat.toValue).get
        println(cnatnat)
        val zero = Zero(loc)
        val app = App(loc, Var(loc, "fun"), zero)
        println(Op.Check.check(app, Value.Nat)(Def("fun", cnatnat.toValue, cfun.toValue)::Γ, r))
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