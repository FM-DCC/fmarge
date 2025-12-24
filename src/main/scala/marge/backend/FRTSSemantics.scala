package marge.backend

import caos.sos.SOS
import marge.syntax.FRTS.{Edge, Edges, QName, FRTS,Action,State}

import scala.annotation.tailrec

object FRTSSemantics extends SOS[Action,FRTS]:

  override def accepting(s: FRTS): Boolean = true // all states are accepting
  /** Calulates the next possible init states */
  def next[Name >: Action](rx: FRTS): Set[(Name, FRTS)] =
    for (st,i) <- rx.inits.data.toSet
        _ <- 1 to i
        (st2, lbl) <- rx.edgs(st) if rx.act((st, st2, lbl))
    yield
      val toAct = rx.on((st,st2,lbl))
      val toDeact = rx.off((st, st2, lbl))
      val newAct = (rx.act ++ toAct) -- toDeact // biased to deactivation
      val newInits = (rx.inits - st) + st2
      lbl -> rx.copy(inits = newInits, act = newAct)

  /** Similar to `next`, but include the full transition instead of only the action name */
  def nextEdge(rx: FRTS): Set[(Edge, FRTS)] =
    for (st,i) <- rx.inits.data.toSet
        _ <- 1 to i
        (st2, lbl) <- rx.edgs(st) if rx.act((st, st2, lbl))
    yield
      val toAct = rx.on((st, st2, lbl))
      val toDeact = rx.on((st, st2, lbl))
      val newAct = (rx.act ++ toAct) -- toDeact // biased to deactivation
      val newInits = (rx.inits - st) + st2
      (st, st2, lbl) -> rx.copy(inits = newInits, act = newAct)

