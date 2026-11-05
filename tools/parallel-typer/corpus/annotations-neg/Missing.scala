package annneg

import scala.annotation.implicitNotFound

@implicitNotFound("No Show for ${T}: define one in its companion")
trait Show[T]
