package Op
import Data.Src.Src
import Data.Gamma.Gamma
import Data.Core.Core
import Data.ErrorInfo
import Data.Value.Value
package object Infer {
    def infer(src: Src)(implicit Γ: Gamma): Either[ErrorInfo, (Core, Value)] = ???
}