package com.research.nomad.markii.analyses

import soot.toolkits.graph.DirectedGraph

case class AnalyzedMethod[A](var entryValue: A, var exitValue: Option[A] = None) { }

case class WorkListItem[M, N](method: M, unit: N, cfg: DirectedGraph[N])

