package Utils

import Data.Gamma._
import Data.Core._

package object Fresh {
    def fresh(implicit gamma: Gamma): String = {
        def generate = (1 until 10).map(_ => (math.random() * 26).toChar + 'a').map(_.toChar).mkString
        var name = generate
        while (gamma has name) {
            name = (1 until 10).map(_ => (math.random() * 26).toInt + 'a').mkString 
        }
        name
    }
}