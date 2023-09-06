WueOrtho Pipeline Format
========================

*This document was generated on 2023-08-02. Use the appropriate main to generate an up-to-date version.*

A WueOrtho pipeline can be assembled using its JSON representation.
The pipeline describes a selection and order of algorithms to produce an orthogonal graph drawing.
Each step may read and write intermediate results to a shared cache to persist results and to promote data to following steps.

Intermediate results are grouped by type into so-called stages.
Currently, there are the following stages:
 - Graph
 - Layout
 - VertexLabels
 - VertexBoxes
 - Ports
 - PortLabels
 - RoutingGraph
 - EdgeRouting
 - Routes
 - Svg
 - Metadata
 - ForeignData

In order to execute pipelines, we provide runtimes with different feature scopes.
Runtimes define which steps a pipeline can have and provide implementations for each step.
This text was compiled for the 'core-with-praline-steps' runtime. Different runtimes may diverge in naming and functionality.
A pipeline for this runtime may produce different results or may not even run with other runtimes.

A pipeline definition is a json object `{ "steps": [<step object>] }`.
Each step is defined by a step object `{ "type": "<step type>" }` with potentially more keys to configure the step's behavior.
For example the angle heuristic used to distribute ports on vertex boxes may be defined as follows.
```
{
  "mode" : "OnlyVertical",
  "type" : "PortsByAngle",
  "tag" : "vertical",
  "graph" : "sample"
}
```
Each step is identified by its `type` key. The PortsByAngle step additionally has a mandatory `mode` key.
The `tag` key is a special key for all Steps that defines an id that is attached to all stages written by this step.
Some steps have input tags (like `graph` above). These can be used to select stages from which this step obtains its inputs.

The following steps are available with this runtime.

**RandomGraph**

Create graphs at random.
 * The PRNG is generated using `seed`.
 * The graph will have `n` vertices and `m` edges.
 * `core` - allows to specify a graph structure for connectivity.
   Possible cores are `Empty`, `Path`, `Tree`, `Star`.
 * `allowLoops` - enable self-edges.

*Input Tags*: -


**RandomVertexBoxes**

Create vertex boxes at random.
 * `minSpan`/`maxSpan` -
   minimum/maximum span vector of the boxes (span.x = width/2, span.y = height/2)
 * `seed` - the PRNG is created using this

*Input Tags*: `graph`


**UniformVertexBoxes**

Create vertex boxes of uniform size.
 * `span` - span vector of the boxes
    (span.x = width/2, span.y = height/2)

*Input Tags*: `vertexLayout`


**SyntheticVertexLabels**

Create artificial vertex labels.
 * `config` - is one of `Hide`, `Enumerate`.

*Input Tags*: `graph`


**SyntheticPortLabels**

Create artificial vertex labels.
 * `config` - is one of `Hide`, `Enumerate`.

*Input Tags*: `ports`


**BoxesFromLabels**

Create vertex boxes to host text labels
 * `config` - a json object with either just
   `{"type": "PralineDefaults"}` or
   `{"type": "Custom"}` and following attributes
   - `minWidth`/`minHeight` -
      minimum width and height.
   - `padding` - at all sides.
   - `fontSize`

*Input Tags*: `vertexLayout`, `vertexLabels`


**ReadTglfFile**

Read inputs in Trivial Graph Layout Format.
 * `path` - read from this file.
 * `use` - select a list of extractors.
    Possible values: `Graph`, `VertexLayout`, `VertexBoxes`, `EdgeRoutes`

*Input Tags*: -


**ForceDirectedLayout**

Perform force-directed vertex layout for a given graph.
 * `seed` - The layout is initialized using a PRNG with this seed.
 * `iterations` - and the algorithm stops after so many steps.
 * `repetitions` - number of layouts will be calculated.
    The algorithm chooses the one with the least straight-line crossings

*Input Tags*: `graph`


**GTreeOverlaps**

Remove overlaps among vertex boxes with the GTree algorithm.
 * `seed` - use a PRNG initialized with this seed.
 * `forceGeneralPosition` -
    manipulate vertex positions afterwards to ensure general position.
 * `stretch` - manipulate the boxes before removing overlaps.
    Use a json object `{"type": "<type>"}` where `<type>` is one of `Original`, `Uniform`, `Scale`, `Padding`, `Replace` and possibly attributes
   - `l` - the scalar (for type `Uniform`)
      or vector (for type `Scale`) by which scaling is performed.
   - `p` - the padding vector that is added to each `span`
   - `width`/ `height` - replace boxes
         

*Input Tags*: `vertexBoxes`


**PortsByAngle**

Distribute ports based on straight-line edges.
 * `mode` - use one of `OnlyVertical`, `OnlyHorizontal`, `Quadrants`, `Octants`

*Input Tags*: `vertexBoxes`, `graph`


**SimplifiedRoutingGraph**

Create a routing graph.
 * `stretch` - manipulate the boxes before routing.
   Use a json object `{"type": "<type>"}` where `<type>` is one of `Original`, `Uniform`, `Scale`, `Padding`, `Replace` and possibly attributes
   - `l` - the scalar (for type `Uniform`)
      or vector (for type `Scale`) by which scaling is performed.
   - `p` - the padding vector that is added to each `span`
   - `width`/ `height` - replace boxes

*Input Tags*: `vertexBoxes`, `ports`


**CenteredRoutingGraph**

Create a routing graph with all edges starting at the centers of vertex boxes.

*Input Tags*: `graph`, `vertexBoxes`


**EdgeRouting**

Perform edge routing (includes edge ordering).

*Input Tags*: `graph`, `routingGraph`


**PseudoRouting**

Produce a fake edge routing from already routed edges
(e.g. in order to apply a nudging step afterwards).

*Input Tags*: `graph`, `vertexBoxes`, `routes`


**PseudoPorts**

Produce a fake ports from an edge routing. These Ports may overlap.

*Input Tags*: `routing`


**NoNudging**

Perform no nudging.

*Input Tags*: `routing`


**ConstrainedNudging**

Perform constrained nudging.

*Input Tags*: `ports`, `vertexBoxes`, `routing`


**FullNudging**

Perform full nudging (moves edge segments, ports, and vertex boxes).
 * `padding` - A minimum object distance is maintained.
 * `use2ndHPass` - enables an additional horizontal pass of full nudging.

*Input Tags*: `graph`, `vertexBoxes`, `routing`


**Metrics**

Calculate metrics.
 * `use` - select a list of metrics. Use `["all"]` to select all metrics.
    Otherwise select a subset of `[Crossings, BoundingBoxArea, ConvexHullArea, TotalEdgeLength, EdgeBends, EdgeLengthVariance, HasLoops, HasMultiEdges, IsConnected, AspectRatio, InterEdgeDistance]`.

*Input Tags*: `graph`, `vertexBoxes`, `routes`


**SvgDrawing**

Draw as SVG.
 * `overridePpu` - override the pixels per unit setting [optional]
 * `config` - use a predefined config:
   - `SmoothEdges` colorful smooth edges (ppu=50).
   - `StraightEdges` colorful straight edges (ppu=50).
   - `Praline` close to Praline but with colorful edges (ppu=1).
   - `Custom` full custom (see wueortho.io.svg.Svg for details).

*Input Tags*: `vertexBoxes`, `routes`, `vertexLabels`, `portLabels`


**SvgToFile**

Save the SVG as `path`

*Input Tags*: `svg`


**ReadPralineFile**

Read praline json from file.
 * `path`
 * `use` - configure what data to load. Options are `Graph`, `VertexLabels`, `VertexLayout`, `VertexBoxes`, `EdgeRoutes`.

*Input Tags*: -


**AccessPraline**

Access the praline API via the ForeignData stage.
 * `use` - configure what data to load. Options are `Graph`, `VertexLabels`, `VertexLayout`, `VertexBoxes`, `EdgeRoutes`.

*Input Tags*: `praline`


**WritePralineFile**

Store pipeline contents to file as praline json.
All available stages will be included. Use undefined tags to exclude stages.

*Input Tags*: `graph`


**StorePraline**

Store pipeline contents to the praline API via the ForeignData stage.
All available stages will be included. Use undefined tags to exclude stages.

*Input Tags*: `praline`, `graph`


**UpdatePraline**

Update the Praline graph in the ForeignData stage with pipeline contents.
All available stages will be included. Use undefined tags to exclude stages.

*Input Tags*: `praline`


**PralineDrawer**

Draw the praline graph from the foreign data stage as svg using the praline drawer with default settings.

*Input Tags*: `praline`


