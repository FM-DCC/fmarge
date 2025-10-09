package marge.syntax

import marge.syntax.Syntax.{Edge, EdgeMap, Edges, RxGraph}

object Show:

  def apply(e: Edge): String =
    s"${e._1}-${e._2}${if e._3.n.nonEmpty then s":${e._3}" else ""}"

  def apply(abc: Edges): String =
    abc.map(apply).mkString(", ")

  private def showEdges(abc: EdgeMap): String =
    apply(for (a, bcs) <- abc.toSet; (b, c) <- bcs yield (a, b, c))


  def apply(rx: RxGraph): String =
    s"[init]  ${rx.inits.mkString(",")}\n[act]   ${apply(rx.act)}\n[edges] ${
      showEdges(rx.edg)
    }\n[on]    ${showEdges(rx.on)}\n[off]   ${showEdges(rx.off)}"

  def simple(rx:RxGraph): String =
    s"[at] ${rx.inits.mkString(",")} [active] ${apply(rx.act)}"
