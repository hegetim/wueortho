package wueortho.tests.layout

import wueortho.data.*, Direction.*
import wueortho.ports.AngleHeuristic
import wueortho.tests.layout.TestUtils.{rawSE, Vec2DMatcher}
import wueortho.util.GraphConversions.simple.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LoopsSpec extends AnyFlatSpec, should.Matchers, Vec2DMatcher:
  lazy val graph = Graph.fromEdges(
    Seq(
      rawSE(0, 1),
      rawSE(1, 2),
      rawSE(2, 3),
      rawSE(0, 3),
      rawSE(2, 2),
    ),
  ).mkBasicGraph

  lazy val boxAtNode2       = Rect2D(Vec2D(0, 0), Vec2D(1, 1))
  lazy val neighborsOfNode2 = IndexedSeq(Vec2D(4, 4), Vec2D(4, 0), Vec2D(0, 0), Vec2D(0, 0))

  "A BasicGraph with a loop at Node 2" `should` "have a normal adjacency list at node 0" in:
    graph(NodeIndex(0)).neighbors shouldEqual IndexedSeq(
      BasicLink(NodeIndex(1), 0),
      BasicLink(NodeIndex(3), 1),
    )

  it `should` "have a normal adjacency list at node 1" in:
    graph(NodeIndex(1)).neighbors shouldEqual IndexedSeq(
      BasicLink(NodeIndex(0), 0),
      BasicLink(NodeIndex(2), 0),
    )

  it `should` "have a adjacency list with duplicates at node 2" in:
    graph(NodeIndex(2)).neighbors shouldEqual IndexedSeq(
      BasicLink(NodeIndex(1), 1),
      BasicLink(NodeIndex(3), 0),
      BasicLink(NodeIndex(2), 3),
      BasicLink(NodeIndex(2), 2),
    )

  it `should` "have a normal adjacency list at node 3" in:
    graph(NodeIndex(3)).neighbors shouldEqual IndexedSeq(
      BasicLink(NodeIndex(2), 1),
      BasicLink(NodeIndex(0), 1),
    )

  it `should` "have a loop edge" in:
    graph.edges should contain(rawSE(2, 2))

  it `should` "have the same number of edges as before" in:
    graph.edges.length shouldBe 5

  "The octant heuristic" `should` "generate ports for graphs with loops" in:
    val barycenter = Vec2D(2, 2)
    val uut        = AngleHeuristic.octantHeuristic(boxAtNode2, neighborsOfNode2, barycenter)

    uut(0)._2 shouldBe East
    uut(0)._1 shouldBe vec(1, 1 / 3.0) +- 1e-6

    uut(1)._2 shouldBe East
    uut(1)._1 shouldBe vec(1, -1 / 3.0) +- 1e-6

    uut(2)._2 shouldBe North
    uut(2)._1 shouldBe vec(1 / 3.0, 1) +- 1e-6

    uut(3)._2 shouldBe North
    uut(3)._1 shouldBe vec(-1 / 3.0, 1) +- 1e-6

  "The quadrant heuristic" `should` "generate ports for graphs with loops" in:
    val uut = AngleHeuristic.quadrantHeuristic(boxAtNode2, neighborsOfNode2)

    uut(0)._2 shouldBe North
    uut(0)._1 shouldBe vec(0.5, 1) +- 1e-6

    uut(1)._2 shouldBe East
    uut(1)._1 shouldBe vec(1, 0) +- 1e-6

    uut(2)._2 shouldBe North
    uut(2)._1 shouldBe vec(0, 1) +- 1e-6

    uut(3)._2 shouldBe North
    uut(3)._1 shouldBe vec(-0.5, 1) +- 1e-6

  "The only vertical heuristic" `should` "generate ports for graphs with loops" in:
    val uut = AngleHeuristic.onlyVertical(boxAtNode2, neighborsOfNode2)

    uut(0)._2 shouldBe North
    uut(0)._1 shouldBe vec(0.0, 1) +- 1e-6

    uut(1)._2 shouldBe South
    uut(1)._1 shouldBe vec(-0.5, -1) +- 1e-6

    uut(2)._2 shouldBe South
    uut(2)._1 shouldBe vec(0.5, -1) +- 1e-6

    uut(3)._2 shouldBe South
    uut(3)._1 shouldBe vec(0.0, -1) +- 1e-6

  lazy val mGraph = Graph.fromEdges(
    Seq(
      rawSE(0, 1),
      rawSE(1, 2),
      rawSE(2, 1),
      rawSE(1, 0),
      rawSE(2, 0),
    ),
  ).mkBasicGraph

  "A sample multi-graph" `should` "have a simple core" in:
    val uut = mGraph.withoutMultiEdges.edges
    uut should have size 3
    uut(0) shouldBe rawSE(0, 1)
    uut(1) shouldBe rawSE(0, 2)
    uut(2) shouldBe rawSE(1, 2)
end LoopsSpec
