package Utils
import Data.Result
import Data.Gamma._
import Data.Value._
package object Conversion {
    // implicit def gammaToEnv(Γ: Gamma) = Γ.toEnv

    implicit def force(value: Value): InstantValue = value.forced
}