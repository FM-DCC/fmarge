package marge.backend

import marge.syntax.RTS
import marge.syntax.RTS.{Edges, QName,Edge,Action,State}
import marge.syntax.Show
import caos.sos.SOS

object AnalyseLTS:


  def randomWalk(rx:RTS, max:Int=5000): (Set[RTS],Int,Edges,List[String]) =
    val states = for (a,bs)<-rx.edgs.toSet; (b,_)<-bs; s<-Set(a,b) yield s
    def aux(next:Set[RTS], done:Set[RTS],
            nEdges:Int, fired:Edges, probs:List[String],
            limit:Int): (Set[RTS],Int,Edges,List[String]) =
      if limit <=0 then
        // error 1: too big
        return (done,nEdges,fired, s"Reached limit - traversed +$max edges."::probs)
      next.headOption match
        case None =>
          val missingStates: Set[State] =
            // (rx.inits.data.keySet ++ fired.map(_._2)).intersect(states) -- done.flatMap(_.inits.data.keySet)
            (for (a,dest)<-rx.edgs.toSet; (b,_)<-dest; s <- Set(a,b) yield s)
              -- done.flatMap(_.inits.data.keySet)
          val missingEdges: Edges =
            (for (a,dests)<-rx.edgs.toSet; (b,c)<-dests yield (a,b,c))
              -- fired
          println(s"[AnalyseLTS] next: ${}: visited ${done.size} states, fired ${fired.size} edges.")
          if missingStates.isEmpty && missingEdges.isEmpty then
            (done, nEdges, fired, probs) // success
          else
            (done, nEdges, fired,
              (if missingStates.nonEmpty // error 2: unreachable states
               then List(s"Unreachable state(s): ${missingStates.mkString(",")}") else Nil) :::
              (if missingEdges.nonEmpty  // error 3: unreachable edges
                then List(s"Unreachable edge(s): ${Show(missingEdges)}") else Nil) ::: probs
            )
        case Some(st) if done contains st =>
          aux(next-st,done,nEdges,fired,probs,limit)
        case Some(st) => //visiting new state
          val more = RTSSemantics.nextEdge(st)
          val nEdges2 = more.size
          val newEdges = more.map(_._1)
          var incons = Set[String]()
          var manyInits = Set[State]()
//          var moreEdges: Edges = Set()
          for e<-newEdges do
//            val (toAct,toDeact) = FRTSSemantics.toOnOff(e, st)
            val toAct   = Rel.get(e,st.on)
            val toDeact = Rel.get(e,st.off)
//            val fromE = FRTSSemantics.from(e,st)
//            moreEdges ++= fromE
            val shared = toAct.intersect(toDeact)
            if shared.nonEmpty then
              //val triggers = FRTSSemantics.from(e,st) -- shared
              incons = incons + s"activating and deactivating `${Show(shared)}` by `${Show(e)}`"
            // val inits = st.inits.data.getOrElse(e._2,0) match
            //   case n if n > 1 => Set(e._2)
            //   case _ => Set.empty[State]
          val inits = for (i,n) <- st.inits.data if n > 1 yield i            
          manyInits = manyInits ++ inits
          var newProbs = probs
          if more.isEmpty then newProbs ::= s"Deadlock found: ${Show.simple(st)}"
          if incons.nonEmpty then newProbs ::= s"Found inconsistency: ${incons.mkString(", ")}"
          if manyInits.nonEmpty then newProbs ::= s"Multiple initial states reached the same state ${manyInits.mkString(", ")}"
          aux((next-st)++more.map(_._2), done+st, nEdges+nEdges2,fired++newEdges,newProbs,limit-nEdges2)

    aux(Set(rx), Set(), 0, Set(), Nil, max)


  def sanify(rx:RTS): RTS = 
    // val (visited, passed, nEdges, complete) = SOS.traverseEdges(RTSSemantics, rx, max=5000)
    // if !complete then
    //   sys.error(s"[Sanity] traversal was not complete, visited ${visited.size} states and ${nEdges} edges.")
    // else
    //   def getState(r:RTS): Set[State] =
    //     r.inits.data.keySet
    //   val realStates = visited.flatMap(getStates)
    //   val realEdges = passed.map(e => (e._1, e._3, e._2))
    //   rx.copy(
    //     edgs = rx.edgs.filter(kv => visited.contains(kv._1))
    //       .map(kv => (kv._1, kv._2.filter( (dst,act) => visited.contains(dst) && passed.contains((kv._1,act,dst)) ))),
    //     inits = rx.inits.filter( (st,_) => visited.contains(st) ),
    //     act = rx.act.filter( e => visited.contains(e._1) && visited.contains(e._2) )
    //   )
    ???