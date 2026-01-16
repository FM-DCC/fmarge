package marge.backend

object Rel:
  type Rel[A,B] = Map[A,Set[B]]
  def empty[A,B] = Map[A,Set[B]]() //.withDefaultValue(Set())
  def get[A,B](a:A, r:Rel[A,B]): Set[B] = r.getOrElse(a,Set())
  def add[A,B](ab:(A,B), r:Rel[A,B]) = r + (ab._1 -> (get(ab._1,r)+(ab._2)))
  def join[A,B](r1:Rel[A,B], r2:Rel[A,B]) = r1 ++ r2.map(ab => ab._1 -> (get(ab._1,r1)++ab._2))
  def join[A,B](r1:Rel[A,B], r2:Set[(A,B)]) = r1 ++ r2.groupBy(_._1).map(ab => ab._1 -> (get(ab._1,r1)++ab._2.map(_._2)))
  def toRel[A,B](s:Set[(A,B)]) = s.groupBy(_._1).map(kv=>(kv._1->(kv._2.map(_._2))))
  def contains[A,B](r:Rel[A,B],a:A): Boolean = get(a,r).nonEmpty

