package wueortho.tests.praline

import wueortho.pipeline.*, PipelineStep.just
import wueortho.interop.{PralinePipelineExtensions as PPE, PralineReader}, PPE.PralineExtractor as Use
import wueortho.data.Labels
import scala.util.Using

import java.nio.file

import org.scalatest.flatspec.AnyFlatSpec
import DebuggingStep.defaultTag

class HybridModeSpec extends AnyFlatSpec:
  lazy val rt    = PPE.InteropRuntime(CoreStep.allImpls ++ PPE.allImpls :+ DebuggingStep.impl)
  lazy val input = Using.resource(getClass.getResourceAsStream("/sample.json").nn): stream =>
    PralineReader.fromInputStream(stream).get

  lazy val debugPrint = DebuggingStep: cache =>
    for
      graph  <- cache.getStageResult(Stage.Graph, defaultTag)
      labels <- cache.getStageResult(Stage.VertexLabels, defaultTag)
      textLs  = labels match
                  case Labels.Hide              => IndexedSeq.empty
                  case Labels.PlainText(labels) => labels
      boxes  <- cache.getStageResult(Stage.VertexBoxes, defaultTag)
      routes <- cache.getStageResult(Stage.Routes, defaultTag)
    yield println:
        s"""Vertices:
           |${(textLs zip boxes.asRects).zipWithIndex.map { case ((s, r), i) => s"$i: $s @ ${r.center}" }.mkString("\n")}
           |
           |Edges:
           |${graph.edges.zipWithIndex.map((e, j) => s"$j: ${textLs(e.from.toInt)} -> ${textLs(e.to.toInt)}")
            .mkString("\n")}
           |
           |Terminals:
           |${routes.zipWithIndex.map((r, k) => s"$k: ${r.terminals}").mkString("\n")}""".stripMargin

  "The WueOrtho pipeline with Praline extensions" `should` "allow constrained nudging with original edge routes" in:
    rt.ref.set(input)
    val pipeline = Pipeline:
        Seq(
          just(PPE.AccessPraline(List(Use.Graph, Use.VertexBoxes, Use.VertexLabels, Use.EdgeRoutes))),
          just(step.SyntheticPortLabels(SyntheticLabels.Hide)),
          just(step.SvgDrawing(SvgConfig.Praline, overridePpu = None)),
          just(step.SvgToFile(file.Path.of("test-results", "praline-original.svg").nn)),
          just(step.PseudoRouting()),
          just(step.PseudoPorts()),
          just(step.ConstrainedNudging()),
          just(step.SvgDrawing(SvgConfig.Praline, overridePpu = None)),
          just(step.SvgToFile(file.Path.of("test-results", "praline-hybrid-constrained.svg").nn)),
        )
    rt.run(pipeline)

  it `should` "allow full nudging with original edge routes" in:
    rt.ref.set(input)
    val pipeline = Pipeline:
        Seq(
          just(PPE.AccessPraline(List(Use.Graph, Use.VertexBoxes, Use.VertexLabels, Use.EdgeRoutes))),
          just(step.SyntheticPortLabels(SyntheticLabels.Hide)),
          just(step.PseudoRouting()),
          just(step.FullNudging(padding = 10, use2ndHPass = true)),
          just(step.SvgDrawing(SvgConfig.Praline, overridePpu = None)),
          just(step.SvgToFile(file.Path.of("test-results", "praline-hybrid-full.svg").nn)),
        )
    rt.run(pipeline)
end HybridModeSpec
