package wueortho.interop

import wueortho.data.*
import wueortho.util.Traverse.traverse

import de.uniwue.informatik.praline.datastructure
import datastructure.{graphs as P, labels as L, paths}, datastructure.oldUnstyledObjects as old,
  datastructure.utils.Serialization

import java.nio.file.{Files, Path as NioPath}
import java.awt.geom.Point2D.Double as AwtPoint
import scala.util.Try
import scala.jdk.CollectionConverters.*
import scala.language.unsafeNulls

object PralineReader:
  object fromString extends Serialization:
    def apply(s: String) = Try(Serialization.mapper.readValue(s, classOf[P.Graph]))

  case class PralineEdgeSorter(edges: Map[P.Edge, (Int, Boolean)]):
    def apply(e: P.Edge) = edges(e)

  def fromFile(path: NioPath) = for
    str <- Try(Files.readString(path))
    g   <- fromString(str)
  yield g

  def fromInputStream(is: java.io.InputStream) = for
    str <- Try(String(is.readAllBytes()))
    g   <- fromString(str)
  yield g

  object syntax:
    extension (g: P.Graph)
      def getBasicGraph   = mkBasicGraph(g)
      def getHypergraph   = mkHypergraph(g)
      def getVertexLabels = mkVertexLabels(g)
      def getVertexBoxes  = mkVertexBoxes(g)
      def getEdgeRoutes   = mkEdgeRoutes(g)

  def mkBasicGraph(g: P.Graph) =
    // def portsFlat(ps: Seq[P.PortComposition]): Either[String, Seq[P.Port]] =
    //   ps.foldLeft(Right(Seq.empty[P.Port]).withLeft[String]):
    //     case (acc, p: P.Port)       => acc.map(_ :+ p)
    //     case (acc, pg: P.PortGroup) =>
    //       acc.flatMap(more => portsFlat(pg.getPortCompositions().asScala.toSeq).map(more ++ _))
    //     case (acc, err)             => Left(s"unsupported port composition: ${err.getClass.getName()}")

    for edges <- mkEdges(g)
    yield edges.map((i, j, _) => NodeIndex(i min j) -> NodeIndex(i max j)).sorted
      .foldLeft(Graph.builder())(_.addEdge.tupled(_)).mkBasicGraph
  end mkBasicGraph

  private def mkEdges(g: P.Graph) =
    val lut = g.getVertices.asScala.zipWithIndex.toMap

    def mkEdge(e: P.Edge) =
      for
        (u, v) <- e.getPorts.asScala.toSeq match
                    case Seq(u, v) => Right(u -> v)
                    case _         => Left(s"$e: dangling edges and hyperedges are unsupported")
        (i, j) <- (lut.get(u.getVertex) zip lut.get(v.getVertex)).toRight(s"could not find vertices $u and $v")
      yield (i, j, e)

    g.getEdges().asScala.toSeq.traverse(mkEdge)
  end mkEdges

  def mkEdgeSorter(g: P.Graph) = for edges <- mkEdges(g)
  yield PralineEdgeSorter:
      val edgesWithIds = edges.sortBy((i, j, _) => (i min j) -> (i max j)).zipWithIndex.map:
        case ((u, v, edge), i) => edge -> (i, u > v)
      edgesWithIds.toMap

  def mkHypergraph(g: P.Graph) =
    val lut = g.getVertices.asScala.zipWithIndex.toMap

    def mkHyperedge(e: P.Edge) = e.getPorts.asScala.toSeq
      .traverse(v => lut.get(v.getVertex).map(NodeIndex(_)).toRight(s"could not find vertex $v"))

    def edgeFromPortPairing(pp: P.PortPairing) = for
      u <- lut.get(pp.getPort0.getVertex).map(NodeIndex(_))
      v <- lut.get(pp.getPort1.getVertex).map(NodeIndex(_))
      if u != v
    yield Seq(u, v)

    val ppEdges = for
      vg <- g.getVertexGroups().asScala.toSeq
      pp <- vg.getPortPairings().asScala
      e  <- edgeFromPortPairing(pp).orElse:
              Console.err.println(s"WARN could not resolve $pp")
              None
    yield e

    g.getEdges.asScala.toSeq.traverse(mkHyperedge)
      .map(edges => (edges ++ ppEdges).foldLeft(Hypergraph.Builder.empty)(_.addEdge(_)).mkHypergraph)
  end mkHypergraph

  def mkVertexLabels(g: P.Graph): Either[String, Labels.PlainText] =
    val plain = g.getVertices.asScala.toSeq.traverse: v =>
      v.getLabelManager.getMainLabel match
        case (l: old.OldUnstyledTextLabel) => Right(l.getInputText)
        case (l: L.TextLabel)              => Right(l.getInputText)
        case err                           => Left(s"unsupported label type: ${err.getClass}")
    plain.map(ls => Labels.PlainText(ls.toIndexedSeq))

  def mkVertexBoxes(g: P.Graph) =
    val rects = g.getVertices.asScala.toSeq.traverse: v =>
      for
        shape <- Option(v.getShape).toRight(s"vertex $v has no shape")
        box   <- Option(shape.getBoundingBox).toRight(s"vertex $v has a shape with no bounding box")
      yield Rect2D(Vec2D(box.getCenterX, -box.getCenterY), Vec2D(box.getWidth / 2, box.getHeight / 2))
    rects.map(rs => VertexBoxes(rs.toIndexedSeq))

  def mkEdgeRoutes(g: P.Graph) = mkEdges(g).flatMap(_.traverse: (i, j, e) =>
    for
      paths <- Option(e.getPaths()).toRight(s"path must not be null ($e)")
      path  <- paths.asScala.toSeq match
                 case Seq(one) => Right(one)
                 case err      => Left(s"expected edge with exactly one path but was $err")
      res   <- path2route(path)
    yield (i min j, i max j) -> (if i <= j then res else res.reverse)).map(_.sortBy(_._1).map(_._2).toIndexedSeq)

  private def path2route(path: paths.Path) = path match
    case p: paths.PolygonalPath =>
      for
        (first, mid, last) <-
          Try((p.getStartPoint(), p.getBendPoints().asScala.toSeq, p.getEndPoint())).toEither.left.map(_.toString())
        ortho              <- points2ortho((first +: mid :+ last).map(_.asVec2D))
      yield EdgeRoute(EdgeTerminals(first.asVec2D, ortho.head.dir, last.asVec2D, ortho.last.dir.reverse), ortho)
    case _                      => Left(s"unsupported path type: ${path.getClass().getSimpleName()}")

  private def points2ortho(ps: Seq[Vec2D]) =
    import EdgeRoute.OrthoSeg.*
    ps.sliding(2).toSeq.traverse:
      case Seq(a, b) =>
        if a == b then Left("empty segments are unsupported")
        else if a.x1 == b.x1 then Right(VSeg(b.x2 - a.x2))
        else if a.x2 == b.x2 then Right(HSeg(b.x1 - a.x1))
        else Left(s"segment from $a to $b is not orthogonal")
  end points2ortho

  extension (p: AwtPoint) private def asVec2D = Vec2D(p.x, -p.y)
end PralineReader
