package wueortho.routing

import wueortho.data.*

import scala.util.Random
import scala.Option.when
import scala.collection.mutable
import scala.annotation.tailrec

case class OVG(nodes: IndexedSeq[OVGNode], private val obstacleLinks: IndexedSeq[Int]):
  def apply(i: NodeIndex)     = nodes(i.toInt)
  def findObstacle(id: Int)   = nodes(obstacleLinks(id))
  val length                  = nodes.length
  val bottomLeftNodeIdx       = NodeIndex(0)
  val obstacleBorder          = OrthogonalVisibilityGraph.obstacleBorder(this)
  val edgeOfWorld             = OrthogonalVisibilityGraph.edgeOfWorld(this)
  val neighbor                = OrthogonalVisibilityGraph.neighbor(this)
  def isPort(id: NodeIndex)   = id.toInt >= length
  def asPortId(id: NodeIndex) = id.toInt - length
end OVG

case class OVGNode(
    left: NavigableLink,
    top: NavigableLink,
    right: NavigableLink,
    bottom: NavigableLink,
    obstacle: Option[Int],
):
  def allLinks = List(left, top, right, bottom)

  def dirToNode(id: NodeIndex) =
    def isLinkToNode(link: NavigableLink) = PartialFunction.cond(link):
      case NavigableLink.Node(other) if other == id => true

    if isLinkToNode(left) then Some(Direction.West)
    else if isLinkToNode(top) then Some(Direction.North)
    else if isLinkToNode(right) then Some(Direction.East)
    else if isLinkToNode(bottom) then Some(Direction.South)
    else None
  end dirToNode

  def dirToPort(id: Int) =
    def isLinkToPort(link: NavigableLink) = PartialFunction.cond(link):
      case NavigableLink.Port(other) if other == id => true

    if isLinkToPort(left) then Some(Direction.West)
    else if isLinkToPort(top) then Some(Direction.North)
    else if isLinkToPort(right) then Some(Direction.East)
    else if isLinkToPort(bottom) then Some(Direction.South)
    else None
  end dirToPort
end OVGNode

enum NavigableLink derives CanEqual:
  case EndOfWorld
  case Node(idx: NodeIndex)
  case Obstacle(idx: Int) // index of the rect array
  case Port(idx: Int)     // ports(x).u = 2 * x, ports(x).v = 2 * x + 1

object OrthogonalVisibilityGraph:
  case class HSegment(fromX: Double, toX: Double, y: Double, origin: Origin)
  case class VSegment(x: Double, fromY: Double, toY: Double, origin: Origin)

  enum Origin:
    case Port(id: Int)
    case Obstacle(dir: Direction, id: Int)

  enum QueueItem extends Positioned1D:
    case Start(pos: Double, rect: Rect2D, idx: Int)
    case Mid(pos: Double, snd: Double, idx: Int)
    case End(pos: Double, rect: Rect2D, idx: Int)

  object QueueItem:
    def fromPort(filter: Direction => Boolean, params: Vec2D => (Double, Double))(t: EdgeTerminals, i: Int) =
      def mkMid(v: Vec2D, i: Int) =
        val (pos, snd) = params(v)
        QueueItem.Mid(pos, snd, i)
      (when(filter(t.uDir))(mkMid(t.uTerm, 2 * i)) :: when(filter(t.vDir))(mkMid(t.vTerm, 2 * i + 1)) :: Nil).flatten

    given Ordering[QueueItem] = Ordering.by((_: QueueItem).pos).orElseBy:
      case _: QueueItem.Start => 2
      case _: QueueItem.Mid   => 1
      case _: QueueItem.End   => 0
  end QueueItem

  enum PartialOVGNode:
    case Init(left: NavigableLink, bottom: NavigableLink, vi: Int, hi: Int, obstacle: Option[Int])
    case WithTop(top: NavigableLink, base: PartialOVGNode.Init)
    case WithRight(right: NavigableLink, base: PartialOVGNode.Init)
    case Ready(node: OVGNode)

    def withTop(top: NavigableLink): PartialOVGNode = this match
      case base: Init            => WithTop(top, base)
      case WithRight(right, b)   => Ready(OVGNode(b.left, top, right, b.bottom, b.obstacle))
      case _: WithTop | _: Ready => sys.error(s"Cannot add bottom part to $this")

    def withRight(right: NavigableLink): PartialOVGNode = this match
      case base: Init              => WithRight(right, base)
      case WithTop(top, b)         => Ready(OVGNode(b.left, top, right, b.bottom, b.obstacle))
      case _: WithRight | _: Ready => sys.error(s"cannot add right part to $this")
  end PartialOVGNode

  def create(rects: IndexedSeq[Rect2D], ports: PortLayout): (WeightedGraph, VertexLayout, IndexedSeq[SimpleEdge], OVG) =
    val (hSegs, vSegs) = buildSegments(rects, ports)
    val (ovg, layout)  = buildGraph(hSegs, vSegs, rects, ports)
    val adj            = adjacencies(ovg, ports)
    val edges          =
      ((ovg.length until adj.numberOfVertices by 2) zip (ovg.length + 1 until adj.numberOfVertices by 2))
        .map((u, v) => SimpleEdge(NodeIndex(u), NodeIndex(v)))
    (adj, layout, edges, ovg)

  def buildSegments(nodes: IndexedSeq[Rect2D], ports: PortLayout) =
    import QueueItem.*

    case class State[S <: HSegment | VSegment](posHP: Set[Double], negHP: Set[Double], segments: List[S])

    def nextNHP(s: State[?], start: Double) = s.negHP.filter(_ >= start).minOption.getOrElse(Double.PositiveInfinity)
    def nextPHP(s: State[?], start: Double) = s.posHP.filter(_ <= start).maxOption.getOrElse(Double.NegativeInfinity)

    val hSegs =
      val queue = (nodes.zipWithIndex.flatMap((rect, i) => List(Start(rect.bottom, rect, i), End(rect.top, rect, i)))
        ++ (ports.byEdge.zipWithIndex flatMap QueueItem.fromPort(_.isHorizontal, v => v.x2 -> v.x1))).sorted

      def mkObsSeg(state: State[HSegment], rect: Rect2D, y: Double, origin: Origin) =
        HSegment(nextPHP(state, rect.left), nextNHP(state, rect.right), y, origin)

      def mkPortSeg(state: State[HSegment], id: Int, x: Double, y: Double) =
        val dir =
          val tmp = ports(id / 2)
          if id % 2 == 0 then tmp.uDir else tmp.vDir
        dir match
          case Direction.East => HSegment(x, nextNHP(state, x), y, Origin.Port(id))
          case Direction.West => HSegment(nextPHP(state, x), x, y, Origin.Port(id))
          case _              => sys.error(s"cannot build a horizontal segment for port with direction $dir")

      queue.foldLeft(State[HSegment](Set.empty, Set.empty, Nil))((s, item) =>
        item match
          case Start(y, rect, idx) =>
            val orig = Origin.Obstacle(Direction.South, idx)
            State(s.posHP + rect.right, s.negHP + rect.left, mkObsSeg(s, rect, rect.bottom, orig) :: s.segments)
          case End(y, rect, idx)   =>
            val orig = Origin.Obstacle(Direction.North, idx)
            State(s.posHP - rect.right, s.negHP - rect.left, mkObsSeg(s, rect, rect.top, orig) :: s.segments)
          case Mid(y, x, idx)      => s.copy(segments = mkPortSeg(s, idx, x, y) :: s.segments),
      ).segments
    end hSegs

    val vSegs =
      val queue = (nodes.zipWithIndex.flatMap((rect, i) => List(Start(rect.left, rect, i), End(rect.right, rect, i)))
        ++ (ports.byEdge.zipWithIndex flatMap QueueItem.fromPort(_.isVertical, v => v.x1 -> v.x2))).sorted

      def mkObsSeg(state: State[VSegment], rect: Rect2D, x: Double, origin: Origin.Obstacle) =
        VSegment(x, nextPHP(state, rect.bottom), nextNHP(state, rect.top), origin)

      def mkPortSeg(state: State[VSegment], id: Int, x: Double, y: Double) =
        val dir =
          val tmp = ports(id / 2)
          if id % 2 == 0 then tmp.uDir else tmp.vDir
        dir match
          case Direction.North => VSegment(x, y, nextNHP(state, y), Origin.Port(id))
          case Direction.South => VSegment(x, nextPHP(state, y), y, Origin.Port(id))
          case _               => sys.error(s"cannot build a vertical segment for port with direction $dir")

      queue.foldLeft(State[VSegment](Set.empty, Set.empty, Nil))((s, item) =>
        item match
          case Start(x, rect, idx) =>
            val orig: Origin.Obstacle = Origin.Obstacle(Direction.West, idx)
            State(s.posHP + rect.top, s.negHP + rect.bottom, mkObsSeg(s, rect, rect.left, orig) :: s.segments)
          case End(x, rect, idx)   =>
            val orig: Origin.Obstacle = Origin.Obstacle(Direction.East, idx)
            State(s.posHP - rect.top, s.negHP - rect.bottom, mkObsSeg(s, rect, rect.right, orig) :: s.segments)
          case Mid(x, y, idx)      => s.copy(segments = mkPortSeg(s, idx, x, y) :: s.segments),
      ).segments
    end vSegs

    hSegs -> vSegs
  end buildSegments

  def buildGraph(horizontal: List[HSegment], vertical: List[VSegment], rects: IndexedSeq[Rect2D], ports: PortLayout) =
    def intersect(h: HSegment, v: VSegment) =
      Option.unless(h.y < v.fromY || h.y > v.toY || v.x < h.fromX || v.x > h.toX)(Vec2D(v.x, h.y))

    def isPort(id: Int)(x: Double, y: Double, dir: Direction) =
      val edge = ports(id / 2)
      if id % 2 == 0 then edge.uTerm.x1 == x && edge.uTerm.x2 == y && edge.uDir == dir
      else edge.vTerm.x1 == x && edge.vTerm.x2 == y && edge.vDir == dir

    def isObstacleBorder(id: Int)(crossing: Vec2D, dir: Direction) =
      val (r, Vec2D(x, y)) = rects(id) -> crossing
      dir match
        case Direction.West  => x == r.left && y < r.top && y > r.bottom
        case Direction.North => y == r.top && x < r.right && x > r.left
        case Direction.East  => x == r.right && y < r.top && y > r.bottom
        case Direction.South => y == r.bottom && x < r.right && x > r.left

    val nodes         = mutable.ArrayBuffer.empty[PartialOVGNode]
    val positions     = mutable.ArrayBuffer.empty[Vec2D]
    val obstacleLinks = mutable.ArrayBuffer.fill(rects.length)(-1)

    // horizontal sweepline --- bottom to top

    val vSegs     = vertical.sortBy(seg => seg.x -> seg.fromY).toIndexedSeq
    val hSegs     = horizontal.sortBy(seg => seg.y -> seg.fromX).toIndexedSeq
    val vPreNodes = mutable.ArrayBuffer.fill(vSegs.length)(-1)
    val hPreNodes = mutable.ArrayBuffer.fill(hSegs.length)(-1)

    for
      vi       <- 0 until vSegs.length
      hi       <- 0 until hSegs.length
      crossing <- intersect(hSegs(hi), vSegs(vi))
    do
      positions += crossing
      val i = nodes.length

      val bottom =
        val bi = vPreNodes(vi)
        if bi == -1 then // there is no bottom node
          vSegs(vi) -> hSegs(hi) match
            case (VSegment(_, fromY, _, _), _) if fromY.isInfinite                            => NavigableLink.EndOfWorld
            case (VSegment(x, y, _, Origin.Port(id)), _) if isPort(id)(x, y, Direction.North) => NavigableLink.Port(id)
            case (_, HSegment(_, _, _, Origin.Obstacle(Direction.North, id)))
                if isObstacleBorder(id)(crossing, Direction.North) =>
              NavigableLink.Obstacle(id)
            case (vs, hs)                                                                     => sys.error(s"Cannot create bottom link for $vs x $hs")
        else
          nodes(bi) = nodes(bi).withTop(NavigableLink.Node(NodeIndex(i)))
          NavigableLink.Node(NodeIndex(bi))
        end if
      end bottom
      vPreNodes(vi) = i

      val left =
        val li = hPreNodes(hi)
        if li == -1 then // there is no left node
          hSegs(hi) -> vSegs(vi) match
            case (HSegment(fromX, _, _, _), _) if fromX.isInfinite                           => NavigableLink.EndOfWorld
            case (HSegment(x, _, y, Origin.Port(id)), _) if isPort(id)(x, y, Direction.East) => NavigableLink.Port(id)
            case (_, VSegment(_, _, _, Origin.Obstacle(Direction.East, id)))
                if isObstacleBorder(id)(crossing, Direction.East) =>
              NavigableLink.Obstacle(id)
            case (hs, vs)                                                                    => sys.error(s"Cannot create left link for $vs x $hs")
        else
          nodes(li) = nodes(li).withRight(NavigableLink.Node(NodeIndex(i)))
          NavigableLink.Node(NodeIndex(li))
        end if
      end left
      hPreNodes(hi) = i

      hSegs(hi) -> vSegs(vi) match
        case (
              HSegment(_, _, _, Origin.Obstacle(Direction.South, ho)),
              VSegment(_, _, _, Origin.Obstacle(Direction.West, vo)),
            ) if ho == vo =>
          obstacleLinks(ho) = i
          nodes += PartialOVGNode.Init(left, bottom, vi, hi, Some(ho))
        case _ =>
          nodes += PartialOVGNode.Init(left, bottom, vi, hi, None)
      end match
    end for

    def mkTop(vi: Int, hi: Int) = // a node has no top node iff it is the topmost node in its channel
      vSegs(vi) -> hSegs(hi) match
        case (VSegment(_, _, toY, _), _) if toY.isInfinite                                => NavigableLink.EndOfWorld
        case (VSegment(x, _, y, Origin.Port(id)), _) if isPort(id)(x, y, Direction.South) => NavigableLink.Port(id)
        case (vs, hs @ HSegment(_, _, _, Origin.Obstacle(Direction.South, id)))
            if isObstacleBorder(id)(intersect(hs, vs).get, Direction.South) =>
          NavigableLink.Obstacle(id)
        case (vs, hs)                                                                     => sys.error(s"Cannot create right link for node at $hs x $vs.")

    def mkRight(vi: Int, hi: Int) = // a node has no right node iff it is the rightmost node in its channel
      hSegs(hi) -> vSegs(vi) match
        case (HSegment(_, toX, _, _), _) if toX.isInfinite                               => NavigableLink.EndOfWorld
        case (HSegment(_, x, y, Origin.Port(id)), _) if isPort(id)(x, y, Direction.West) => NavigableLink.Port(id)
        case (hs, vs @ VSegment(_, _, _, Origin.Obstacle(Direction.West, id)))
            if isObstacleBorder(id)(intersect(hs, vs).get, Direction.West) =>
          NavigableLink.Obstacle(id)
        case (hs, vs)                                                                    => sys.error(s"Cannot create right link for node at $hs x $vs.")

    val finalNodes = nodes.map {
      case PartialOVGNode.Ready(res)                      => res
      case PartialOVGNode.WithTop(top, b)                 => OVGNode(b.left, top, mkRight(b.vi, b.hi), b.bottom, b.obstacle)
      case PartialOVGNode.WithRight(right, b)             => OVGNode(b.left, mkTop(b.vi, b.hi), right, b.bottom, b.obstacle)
      case PartialOVGNode.Init(left, bottom, vi, hi, obs) => OVGNode(left, mkTop(vi, hi), mkRight(vi, hi), bottom, obs)
    }

    val layout = VertexLayout((positions ++ ports.toVertexLayout.nodes).toIndexedSeq)

    assert(!obstacleLinks.contains(-1), s"missing obstacle link at ${obstacleLinks.indexOf(-1)}")

    (OVG(finalNodes.toIndexedSeq, obstacleLinks.toIndexedSeq), layout)
  end buildGraph

  def adjacencies(ovg: OVG, ports: PortLayout): WeightedGraph =
    val rand       = Random(0x99c0ffee)
    val portOffset = ovg.length
    val builder    = Graph.Builder.reserve(portOffset + ports.numberOfPorts)
    for
      (node, i) <- ovg.nodes.zipWithIndex
      link      <- node.allLinks
    do
      link match
        case NavigableLink.Node(idx) if idx.toInt < i => builder.addEdge(NodeIndex(i), idx, rand.nextDouble())
        case NavigableLink.Port(idx)                  => builder.addEdge(NodeIndex(i), NodeIndex(idx + portOffset), rand.nextDouble())
        case _                                        =>
    end for
    builder.mkWeightedGraph
  end adjacencies

  def obstacleBorder(ovg: OVG)(dir: Direction, oid: Int) =
    import NavigableLink.*, Direction.*

    def nodeOrElse[T](link: NavigableLink)(f: NodeIndex => T, orElse: NavigableLink => T) = link match
      case Node(i) => f(i)
      case link    => orElse(link)

    def failNextNode(i: NodeIndex, link: NavigableLink)    =
      sys.error(s"node $i: ${ovg(i)} should reach next node around obstacle $oid but link was $link")
    def failCornerNode(node: OVGNode, link: NavigableLink) =
      sys.error(s"node $node is not the bottom left corner of obstacle $oid (right link was $link)")

    def walk(inwards: OVGNode => NavigableLink, ahead: OVGNode => NavigableLink)(i: NodeIndex): NodeIndex =
      nodeOrElse(inwards(ovg(i)))(
        identity,
        _ => nodeOrElse(ahead(ovg(i)))(next => walk(inwards, ahead)(next), failNextNode(i, _)),
      )

    def listAll(inwards: OVGNode => NavigableLink, ahead: OVGNode => NavigableLink)(
        res: List[NodeIndex],
        i: NodeIndex,
    ): List[NodeIndex] = inwards(ovg(i)) match
      case EndOfWorld => sys.error(s"node $i: ${ovg(i)} reached end of world")
      case Node(_)    => res.reverse
      case _          => nodeOrElse(ahead(ovg(i)))(listAll(inwards, ahead)(i :: res, _), failNextNode(i, _))

    val bl = ovg.findObstacle(oid)
    dir match
      case North =>
        val first = nodeOrElse(bl.top)(walk(_.right, _.top), failCornerNode(bl, _))
        listAll(_.bottom, _.right)(Nil, first)
      case East  =>
        val first = nodeOrElse(bl.right)(walk(_.top, _.right), failCornerNode(bl, _))
        listAll(_.left, _.top)(Nil, first)
      case South =>
        val first = nodeOrElse(bl.top)(identity, failCornerNode(bl, _))
        listAll(_.top, _.right)(Nil, first)
      case West  =>
        val first = nodeOrElse(bl.right)(identity, failCornerNode(bl, _))
        listAll(_.right, _.top)(Nil, first)
    end match
  end obstacleBorder

  def edgeOfWorld(ovg: OVG)(dir: Direction) =
    import NavigableLink.*, Direction.*

    @tailrec def go(ahead: NodeIndex => NavigableLink)(res: List[NodeIndex], i: NodeIndex): List[NodeIndex] =
      ahead(i) match
        case EndOfWorld => res.reverse
        case Node(next) => go(ahead)(i :: res, next)
        case err        => sys.error(s"a $dir edge of world node should not have a $err link")

    dir match
      case North | East => ???
      case South        => go(ovg(_).right)(List(ovg.bottomLeftNodeIdx), ovg.bottomLeftNodeIdx)
      case West         => go(ovg(_).top)(List(ovg.bottomLeftNodeIdx), ovg.bottomLeftNodeIdx)
  end edgeOfWorld

  def neighbor(ovg: OVG)(id: NodeIndex, dir: Direction) =
    val node = dir match
      case Direction.North => ovg(id).top
      case Direction.East  => ovg(id).right
      case Direction.South => ovg(id).bottom
      case Direction.West  => ovg(id).left
    node match
      case NavigableLink.EndOfWorld    => None
      case NavigableLink.Node(idx)     => Some(idx.toInt)
      case NavigableLink.Obstacle(idx) => None
      case NavigableLink.Port(idx)     => Some(ovg.length + idx)
  end neighbor

  class RoutingGraphAdapter(ovg: OVG, adj: WeightedGraph, lay: VertexLayout, ports: PortLayout) extends RoutingGraph:

    override def portId(node: NodeIndex) = Option.when(ovg.isPort(node))(ovg.asPortId(node))

    override def neighbor(node: NodeIndex, dir: Direction) =
      if ovg.isPort(node) then portNeighbor(node, dir) else ovg.neighbor(node, dir).map(NodeIndex(_))

    override def neighbors(node: NodeIndex) = if ovg.isPort(node) then portNeighbors(node)
    else Direction.values.toList.flatMap(dir => ovg.neighbor(node, dir).map(dir -> NodeIndex(_)))

    override def resolveEdge(edgeId: Int) = NodeIndex(ovg.length + 2 * edgeId) -> NodeIndex(ovg.length + 2 * edgeId + 1)

    override def size = ovg.length + ports.numberOfPorts

    override def locate(node: NodeIndex) = lay(node)

    override def isBlocked(node: NodeIndex) = false

    private def portNeighbor(node: NodeIndex, dir: Direction) =
      Option.when(ports.portDir(ovg.asPortId(node)) == dir)(adj(node).neighbors.head.toNode)

    private def portNeighbors(node: NodeIndex) =
      adj(node).neighbors.map(link => ports.portDir(ovg.asPortId(node)) -> link.toNode).toList
  end RoutingGraphAdapter

end OrthogonalVisibilityGraph
