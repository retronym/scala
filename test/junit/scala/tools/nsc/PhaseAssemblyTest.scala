package scala.tools.nsc

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class PhaseAssemblyTest {

  @Test
  def initDefinitions = {
    val global = new Global(new Settings())
    import global._
    val graph = new DependencyGraph
    val init = graph.getNodeByPhase("init")
    val terminal = graph.getNodeByPhase("term")
    val penultimate = graph.getNodeByPhase("pen")
    graph.softConnectNodes(terminal, penultimate)
    for (i <- 0 to 50; j <- 0 to 1) {
      val n = graph.getNodeByPhase(s"${i}_${j}")
      if (i == 0)
        graph.softConnectNodes(n, init)
      else
        graph.softConnectNodes(n, graph.getNodeByPhase(s"${i - 1}_0"))
      graph.softConnectNodes(penultimate, init)
    }
    graph.validateAndEnforceHardlinks()
    graph.collapseHardLinksAndLevels(init, 1)
    println(graph.nodes.values.toList filter (_.level > 0) sortBy (x => (x.level, x.phasename)))
  }
}
