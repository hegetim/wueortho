KNOWN ISSUES
============

* [BUG] one separate run of the geo-nudging might put two segments in the same position that do not have each other in their `pathsBefore` sets. This can mean that we introduce unnecessary eyes (fixed for: knock-knee situations)
* [BUG] segments get pressed into vertex boxes by EndOfWorld nodes (fixed for full nudging)
* [BUG] nonce is always zero in routing
* [POTENTIAL BUG] multiple monotony constraints might replace a separation constraint but do not induce a margin (we may be able to show that this cannot happen)
* [POTENTIAL BUG] margins between segments of the same path might be desired (fixed for loops)
* [IMPROVEMENT] we should add pseudo constraint edges for port -- port and port -- begin/end of vertex box pairs.
* [IMPROVEMENT] close fixed segments should be joined before routing. Routed paths need to be separated (e.g. by adding an additional bend to one of them)
