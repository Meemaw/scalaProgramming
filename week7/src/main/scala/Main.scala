

case class AlgebraStruct[A](s: List[A], op: (A, A) => A) {

  //enota te strukture, ce jo ta ima
  //val unit: Option[A] = ???

  // definiran operator <=>
  implicit class Op(val b:A) {
    def <=> (that: A) = op(that,this.b)
  }

  // Ko boste imeli definirane spodnje metode, določite še te vrednosti.
  val isGrupoid: Boolean = isClosed
  val isSemigroup: Boolean = isClosed && isAssoc
  val isMonoid: Boolean = isClosed && isAssoc && hasUnit
  val isGroup: Boolean = isClosed && isAssoc && hasUnit && s.forall(x => hasInverse(x))
  val isAbelianGroup: Boolean = isCommut
  
  
  lazy val pairs:List[(A,A)] = for{x <- s; y <- s} yield (x,y)
  lazy val triplets:List[(A,A,A)] = for{x <- s; y <- s; z <- s} yield (x,y,z)
  lazy val unit:Option[A] = {
    val kandidati = pairs.filter(x => (x._1 <=> x._2) == x._2).map(x => x._1)
    val c = kandidati.filter(kandidat => s.forall(y => op(y, kandidat) == y))
    if(c.isEmpty) None
    else Option(c.head)
  }

  def isClosed: Boolean = pairs.map(x => (x._1 <=> x._2)).forall(x => s.contains(x))

  def isAssoc: Boolean = triplets.forall(x => op(op(x._1,x._2), x._3) == op(x._1, op(x._2, x._3)))

  def hasUnit: Boolean = unit.isDefined

  def hasInverse(e: A): Boolean = unit match {
    case None => false
    case Some(enota) => s.exists(x => (e <=> x) == enota && enota == (x <=> e)) 
  }

  def isCommut: Boolean = pairs.forall(x => (x._1 <=> x._2) == (x._2 <=> x._1))

  //Ce ta struktura ni grupoid, vrnite nov grupoid, ki ga dobite tako, da
  //razširite množico s, da postane op notranja operacija.
  def makeGrupoid: AlgebraStruct[A] = {


    def tryClose(acc:Int, sl:List[A]):AlgebraStruct[A] = {
      val pairs:List[(A,A)] = for{x <- sl; y <- sl} yield (x,y)
      if(acc > 100) throw new Exception("Cant close.") // da se ne zaciklamo
      else if(AlgebraStruct(sl,op).isClosed) AlgebraStruct(sl, op)
      else tryClose(acc+1, pairs.map(x => (x._1 <=> x._2)).filter(x => !sl.contains(x)).distinct ::: sl)
    }
    tryClose(0,s)
  }

}




object RList {
  def apply[A](e: A*): RList[A] = {
    e.foldRight(LEnd: RList[A])((x, l) => l.add(x))
  }
}

trait RList[+A] {
  def add[AA >: A](e: AA): RList[AA]
  def apply(i: Int): A
  def update[AA >: A](idx: Int, newVal: AA): RList[AA]
  val length: Int
  def head: A
  def tail: RList[A]
  override def toString: String
  def toList: List[A]
  def toList1: List[A]
}

/*
* e je element, ki ga vozlišče hrani, next je naslednje drevo v seznamu, l (r) sta levo (desno) poddrevo
* se pa je število elementov v tem drevesu (vključno s korenom).
*/
case class LNode[+A](e: A, next: RList[A], l: RList[A], r: RList[A], se: Int) extends RList[A] {

  def toList:List[A] = {
    head :: l.toList1 ::: r.toList1 ::: next.toList
  }

  def toList1:List[A] = {
    head :: l.toList1 ::: r.toList1
  }

  override def toString = toList.toString
  val length = se + next.length
  def head = e

  def add[AA >: A](el: AA): RList[AA] = {
    val x = new LNode(el, this, LEnd, LEnd, 1)
    next match {
      case LNode(_, nextNext@LNode(_,_,_,_,s2), _, _, s1) => if(s1 == s2) new LNode(el, nextNext, this, next, 1+s1+s2) else x
      case _ => x
    }
  }

  def apply(i:Int) = {
    if(i < 0) throw new Exception("i < 0")
    else if(i == 0) e
    else if(i < se) {
      l match {
        case LNode(_,_,_,_,sl) => if(i - 1  < sl) l.apply(i-1) else r.apply(i-1-sl)
      }
    }
    else next.apply(i-se)
  }
  def tail = this match {
    case LNode(_,_,LEnd,LEnd,_) => next
    case _ => l
  }
 
  def update[AA >: A](i: Int, newVal: AA): RList[AA] = {
    if(i < 0) throw new Exception("i < 0")
    else if(i == 0) new LNode(newVal, next, l ,r ,se)
    else if(i < se) {
      l match {
        case LNode(_,_,_,_,sl) => if(i - 1 < sl) new LNode(e, next, l.update(i-1, newVal),r, se) else new LNode(e, next, l, r.update(i-1-sl, newVal), se)
      }
    }
    else new LNode(e, next.update(i-se, newVal), l, r, se)
  }

}

case object LEnd extends RList[Nothing] {
  val length = 0
  def add[A](x: A) : RList[A] = new LNode(x, LEnd, LEnd, LEnd, 1)
  def head = throw new Exception
  def apply(i: Int) = throw new Exception
  def toList = Nil
  def toList1 = Nil
  override def toString =toList.toString
  def tail = LEnd
  def update[A](idx: Int, newVal: A): RList[A] = throw new Exception("Should not be there")


}

