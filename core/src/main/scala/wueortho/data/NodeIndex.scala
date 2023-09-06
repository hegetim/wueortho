package wueortho.data

opaque type NodeIndex = Int

object NodeIndex:
  def apply(i: Int): NodeIndex         =
    assert(i >= 0, s"A NodeIndex must be nonnegative but was $i")
    i
  given ord: Ordering[NodeIndex]       = Ordering.Int.on(i => i)
  given CanEqual[NodeIndex, NodeIndex] = CanEqual.derived

  extension (i: NodeIndex)
    inline def toInt: Int = i

    infix def to(j: NodeIndex | Int): IndexedSeq[NodeIndex]    = Range.inclusive(i, j).map(NodeIndex(_))
    infix def until(j: NodeIndex | Int): IndexedSeq[NodeIndex] = Range(i, j).map(NodeIndex(_))
