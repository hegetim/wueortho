package wueortho.util

import wueortho.data.*
import scala.collection.mutable

object MinimumSpanningTree:
  private enum VertexState derives CanEqual:
    case Root
    case Undiscovered
    case Discovered(key: Double, pred: NodeIndex)
    case Bound(weight: Double, pred: NodeIndex)

    def isStillUnbound = this match
      case Bound(_, _) => false
      case _           => true

    def isCandidate(w: Double) = this match
      case Root | Bound(_, _) => false
      case Undiscovered       => true
      case Discovered(key, _) => w > key

    def bind = this match
      case Root | Bound(_, _) => this
      case Discovered(w, p)   => VertexState.Bound(w, p)
      case Undiscovered       => sys.error(s"cannot bind undiscovered vertex $this")
  end VertexState

  def create(g: WeightedGraph): WeightedDiGraph =
    val state = mutable.ArraySeq.fill(g.vertices.size)(VertexState.Undiscovered)
    state(0) = VertexState.Root
    val queue = mutable.PriorityQueue(0.0 -> NodeIndex(0))

    while queue.nonEmpty do
      val (_, u) = queue.dequeue()
      if state(u.toInt).isStillUnbound then
        g.vertices(u.toInt).neighbors.foreach:
          case WeightedLink(v, weight, _) =>
            if state(v.toInt).isCandidate(-weight) then
              state(v.toInt) = VertexState.Discovered(-weight, u)
              queue.enqueue(-weight -> v)
        state(u.toInt) = state(u.toInt).bind
    end while

    mkTree(state.toSeq)
  end create

  private def mkTree(s: Seq[VertexState]) =
    val builder = Graph.DiBuilder.reserve(s.size)
    s.zipWithIndex.foreach:
      case (VertexState.Bound(w, u), v) => builder.addEdge(u, NodeIndex(v), -w)
      case (VertexState.Root, i)        => // the root has no incoming edges
      case x                            => sys.error(s"unbound vertex $x in MST")
    builder.mkWeightedDiGraph

end MinimumSpanningTree
