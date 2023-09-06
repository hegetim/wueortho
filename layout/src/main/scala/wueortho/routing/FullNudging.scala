package wueortho.routing

import wueortho.data.*, Direction.*
import wueortho.util.*, GraphConversions.undirected.*
import Constraint.CTerm, Constraint.builder.*, ORTools.LPResult

import scala.annotation.tailrec

class FullNudging(val conf: Nudging.Config) extends NudgingCommons:

  private def segBuilder(pathId: Int, rg: RoutingGraph & PathOrder, vertexBoxes: IndexedSeq[BoxNodes]): SegmentBuilder =
    def termBorder(t: Terminal) = t.dir match
      case North => vertexBoxes(t.boxId).top.pos
      case East  => vertexBoxes(t.boxId).right.pos
      case South => vertexBoxes(t.boxId).bottom.pos
      case West  => vertexBoxes(t.boxId).left.pos

    new SegmentBuilder(pathId, rg):
      import Segment.*
      override def mkOne(gs: Segment.SegInRG, t1: Terminal, t2: Terminal) = ???
      override def mkTT(gs: Segment.SegInRG, t1: Terminal, t2: Terminal)  = State((xv, yv) =>
        if gs.isH then (xv, yv + 1) -> CNode(mkVar(yv), mkEst(gs), TermSeg(termBorder(t1), mkInfo(gs, mkVar(xv))))
        else (xv + 1, yv)           -> CNode(mkVar(xv), mkEst(gs), TermSeg(termBorder(t1), mkInfo(gs, mkVar(yv)))),
      )
      override def mkTN(gs: Segment.SegInRG, t: Terminal)                 = State((xv, yv) =>
        if gs.isH then (xv, yv + 1) -> CNode(mkVar(yv), mkEst(gs), TermSeg(termBorder(t), mkInfo(gs, termBorder(t))))
        else (xv + 1, yv)           -> CNode(mkVar(xv), mkEst(gs), TermSeg(termBorder(t), mkInfo(gs, termBorder(t)))),
      )
      override def mkTM(gs: Segment.SegInRG, t: Terminal)                 = mkTT(gs, t, t)
      override def mkMT(gs: Segment.SegInRG, t: Terminal)                 = mkMM(gs)
      override def mkMM(gs: Segment.SegInRG)                              = State((xv, yv) =>
        if gs.isH then (xv, yv + 1) -> CNode(mkVar(yv), mkEst(gs), MidSeg(mkInfo(gs, mkVar(xv))))
        else (xv + 1, yv)           -> CNode(mkVar(xv), mkEst(gs), MidSeg(mkInfo(gs, mkVar(yv)))),
      )
    end new
  end segBuilder

  private trait CGraph(boxNodes: IndexedSeq[BoxNodes], paths: IndexedSeq[PathNodes]) extends CGraphCommons:
    def paddingConstraints: Seq[Constraint] =
      graph.edges.map(e => mkConstraint(allNodes(e.to.toInt), allNodes(e.from.toInt), mkConst(conf.padding))._1)

    def homogeneityConstraints: S[(Seq[Constraint], CTerm, Int)] =
      val perComp = split(graph.undirected, allNodes).map(mkConstraintsForComponent(graph, _, allNodes, isHorizontal))
      perComp.toList.sequence.map(tmp =>
        val (cs, obj) = tmp.unzip
        (cs.flatten, obj.foldLeft(mkConst(0))(_ + _), obj.size),
      )

    def boxConstraints: (Seq[Constraint], CTerm) =
      val (cs, obj) = boxNodes.map(o =>
        if isHorizontal then
          (o.left.pos + mkConst(o.right.dim.at - o.left.dim.at) <= o.right.pos)    -> (o.left.pos - o.right.pos)
        else (o.bottom.pos + mkConst(o.top.dim.at - o.bottom.dim.at) <= o.top.pos) -> (o.bottom.pos - o.top.pos),
      ).unzip
      cs -> obj.reduce(_ + _)

    def pathLength: (CTerm, Int) =
      def mkLen(start: CTerm, end: CTerm, dir: Direction) = dir match
        case North | East => end - start
        case South | West => start - end

      @tailrec
      def go(res: CTerm, rem: CTerm, start: CTerm, count: Int, dir: Direction, q: List[CNode[Segment]]): (CTerm, Int) =
        q match
          case Nil          => res + mkLen(start, rem, dir) -> (count + 1)
          case head :: next =>
            if head.kind.isHorizontal != isHorizontal then go(res, rem, start, count, dir, next)
            else if head.kind.info.dir == dir then go(res, head.kind.info.endsAt, start, count, dir, next)
            else go(res + mkLen(start, rem, dir), head.kind.info.endsAt, rem, count + 1, head.kind.info.dir, next)

      paths.map(p =>
        val s   = if isHorizontal == p.u.kind.isHorizontal then p.u.kind.terminal else p.u.pos
        val dir =
          if isHorizontal == p.u.kind.isHorizontal then p.u.kind.info.dir
          else if p.mid.nonEmpty then p.mid.head.kind.info.dir
          else p.v.kind.info.dir
        go(mkConst(0), s, s, 0, dir, p.toList),
      ).reduce((a, b) => (a._1 + b._1, a._2 + b._2))
    end pathLength

    def mkConstraints: S[(Seq[Constraint], CTerm)] =
      val (bCs, bObj) = borderConstraints
      val pCs         = paddingConstraints
      val (oCs, oObj) = boxConstraints
      val (lObj, w1)  = pathLength
      for (sCs, sObj, w2) <- homogeneityConstraints
      yield (bCs ++ pCs ++ oCs ++ sCs) -> (2.0 * (w1 + w2 + 1.0) * (bObj + oObj) + sObj - 2.0 * lObj)

  end CGraph

  private def mkBoxNodes(r: Rect2D, i: Int): S[BoxNodes] = State((xv, yv) =>
    (xv + 2, yv + 2) -> BoxNodes(
      CNode(mkVar(xv), Estimated(r.left, r.bottom, r.top), BoxBorder.Begin(i)),
      CNode(mkVar(xv + 1), Estimated(r.right, r.bottom, r.top), BoxBorder.End(i)),
      CNode(mkVar(yv), Estimated(r.bottom, r.left, r.right), BoxBorder.Begin(i)),
      CNode(mkVar(yv + 1), Estimated(r.top, r.left, r.right), BoxBorder.End(i)),
    ),
  )

  private class HGraph(
      override val eow: (CNode[EndOfWorld], CNode[EndOfWorld]),
      pathNodes: IndexedSeq[PathNodes],
      boxNodes: IndexedSeq[BoxNodes],
  ) extends CGraph(boxNodes, pathNodes):
    override val isHorizontal  = true
    override lazy val segments = pathNodes.flatMap(_.toList.filter(_.kind.isVertical))
    override lazy val boxes    = boxNodes.flatMap(o => Vector(o.left, o.right))

  private def mkHGraph(paths: IndexedSeq[PathNodes], vertexBoxes: IndexedSeq[BoxNodes]): S[HGraph] = for
    (xv, yv) <- State.get[(Int, Int)]
    _        <- State.set((xv + 1, yv))
  yield HGraph((EndOfWorld.mkNode(mkConst(0), West), EndOfWorld.mkNode(mkVar(xv), East)), paths, vertexBoxes)

  private class VGraph(
      override val eow: (CNode[EndOfWorld], CNode[EndOfWorld]),
      pathNodes: IndexedSeq[PathNodes],
      boxNodes: IndexedSeq[BoxNodes],
      xSols: LPResult, // solved horizontal constraints
  ) extends CGraph(boxNodes, pathNodes):
    override val isHorizontal = false

    override lazy val boxes = boxNodes.flatMap(o =>
      Vector(
        o.bottom.copy(dim = o.bottom.dim.copy(low = xSols(o.left.pos), high = xSols(o.right.pos))),
        o.top.copy(dim = o.top.dim.copy(low = xSols(o.left.pos), high = xSols(o.right.pos))),
      ),
    )

    override lazy val segments =
      @tailrec
      def fromPath(queue: List[CNode[Segment]], start: Double, res: List[CNode[Segment]]): Seq[CNode[Segment]] =
        queue match
          case Nil          => res.reverse
          case head :: next =>
            if head.kind.isVertical then fromPath(next, start, res)
            else
              val (end, s) = setX(head, start, xSols)
              fromPath(next, end, s :: res)
      pathNodes.flatMap(p => fromPath(p.toList, xSols(p.startX), Nil))
    end segments

  end VGraph

  private def mkVGraph(paths: IndexedSeq[PathNodes], vertexBoxes: IndexedSeq[BoxNodes], xSols: LPResult): S[VGraph] =
    for
      (xv, yv) <- State.get[(Int, Int)]
      _        <- State.set((xv, yv + 1))
    yield VGraph((EndOfWorld.mkNode(mkConst(0), South), EndOfWorld.mkNode(mkVar(yv), North)), paths, vertexBoxes, xSols)

  private class HGraph2ndPass(
      override val eow: (CNode[EndOfWorld], CNode[EndOfWorld]),
      pathNodes: IndexedSeq[PathNodes],
      boxNodes: IndexedSeq[BoxNodes],
      xSols: LPResult, // solved horizontal constraints
      ySols: LPResult, // solved vertical constraints
  ) extends CGraph(boxNodes, pathNodes):
    val eps = 1e-8

    override def isHorizontal = true
    override def overscan     = conf.padding - eps

    override lazy val boxes = boxNodes.flatMap(o =>
      Vector(
        o.left.copy(dim = Estimated(xSols(o.left.pos), ySols(o.bottom.pos), ySols(o.top.pos))),
        o.right.copy(dim = Estimated(xSols(o.right.pos), ySols(o.bottom.pos), ySols(o.top.pos))),
      ),
    )

    override lazy val segments =
      @tailrec
      def fromPath(queue: List[CNode[Segment]], start: Double, res: List[CNode[Segment]]): Seq[CNode[Segment]] =
        queue match
          case Nil          => res.reverse
          case head :: next =>
            if head.kind.isHorizontal then fromPath(next, start, res)
            else
              val (end, s) = setY(head, start, xSols(head.pos), ySols)
              fromPath(next, end, s :: res)
      pathNodes.flatMap(p => fromPath(p.toList, ySols(p.startY), Nil))
    end segments
  end HGraph2ndPass

  private def mkTerminals(pl: PortLayout, g: BasicGraph) = (pl.byEdge zip g.edges)
    .flatMap((et, se) => List(Terminal(et.uTerm, et.uDir, se.from.toInt), Terminal(et.vTerm, et.vDir, se.to.toInt)))

  private def mkPorts(routes: IndexedSeq[EdgeRoute]) = PortLayout(routes.map(_.terminals))

  private def mkVertexBoxes(boxNodes: IndexedSeq[BoxNodes], xSols: LPResult, ySols: LPResult) =
    def nodes2rect(o: BoxNodes) = Rect2D.boundingBox(
      List(Vec2D(xSols(o.left.pos), ySols(o.bottom.pos)), Vec2D(xSols(o.right.pos), ySols(o.top.pos))),
    )
    VertexBoxes(boxNodes.map(nodes2rect))

  def calcAll(routing: Routed, graph: BasicGraph, vertexBoxes: VertexBoxes) = (for
    boxNodes <- vertexBoxes.asRects.zipWithIndex.map(mkBoxNodes.tupled).toVector.sequence
    paths    <- Segment.mkAll(routing.paths, routing, mkTerminals(routing.ports, graph), segBuilder(_, routing, boxNodes))
    hGraph   <- mkHGraph(paths, boxNodes)
    xSols1   <- hGraph.mkConstraints.map(maximize)
    ySols    <- mkVGraph(paths, boxNodes, xSols1).flatMap(_.mkConstraints).map(maximize)
    xSols    <-
      if conf.use2ndHPass then HGraph2ndPass(hGraph.eow, paths, boxNodes, xSols1, ySols).mkConstraints.map(maximize)
      else State.pure(xSols1)
  yield
    val routes = mkRoutes(xSols, ySols, paths)
    (routes, mkPorts(routes), mkVertexBoxes(boxNodes, xSols, ySols))
  ).runA(0 -> 0)

end FullNudging

object FullNudging:
  def apply(config: Nudging.Config, routing: Routed, graph: BasicGraph, vertexBoxes: VertexBoxes) =
    new FullNudging(config).calcAll(routing, graph, vertexBoxes)
