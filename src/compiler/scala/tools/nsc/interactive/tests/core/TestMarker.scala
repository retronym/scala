package scala.tools.nsc.interactive.tests.core

case class DuplicateTestMarker(msg: String) extends Exception(msg)

object TestMarker {
  import scala.collection.mutable.Map
  private val markers: Map[String, TestMarker] = Map.empty

  private def checkForDuplicate(marker: TestMarker) {
    markers.get(marker.marker) match {
      case None => markers(marker.marker) = marker
      case Some(otherMarker) =>
        val msg = s"Marker `${marker.marker}` is already used by $marker. Please choose a different marker for $otherMarker"
        throw new DuplicateTestMarker(msg)
    }
  }
}

abstract case class TestMarker(marker: String) {
  TestMarker.checkForDuplicate(this)
}

object CompletionMarker extends TestMarker("/*!*/")

object TypeMarker extends TestMarker("/*?*/")

object HyperlinkMarker extends TestMarker("/*#*/")
