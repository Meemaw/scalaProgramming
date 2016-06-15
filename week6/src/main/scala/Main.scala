

// Naloga 1. Implementacija neučinkovite in učinkovite vrste

trait Q1 {

  // Implementacija kjer je enQ O(n) in deQ O(1)
  def enQ(e:List[Int]):Q1 = 
	  this match{
	  	case QEmpty1 => QNode1(e, QEmpty1)
	  	case QNode1(el, next) => QNode1(el, next.enQ(e))
	  }

  def deQ:(Q1, List[Int]) = this match{
    case QEmpty1 => throw new Exception
    case QNode1(el, next) => (next, el)
  }


}

case object QEmpty1 extends Q1
case class QNode1(el:List[Int], next:Q1) extends Q1

object Q1{

  //  >>>>>>>>>>>>> !!!!!!!!!! Vrednosti bodo verjetno drugačne od vaših, saj je queue implementiran z O(n) enQ in O(1) deQ !!!!!!!!!!! <<<<<<<<

  def countAcc(ops: List[Option[List[Int]]]):Int = 
  {
      def helper(acc:Int, currentSize:Int, left: List[Option[List[Int]]]):Int = left match {
        case Nil => acc
        case Some(x) :: tail => helper(acc + currentSize, currentSize+1, left.tail)
        case _ => helper(acc+1, currentSize-1, left.tail)
      }

      helper(0,0,ops)
  }

}

trait Q2{
  def enQ(e:List[Int]):Q2 = this match {
    case QEmpty2 => QNode2(List(e), Nil)
    case QNode2(front, back) => QNode2(e :: front, back)
  }
  def deQ:(Q2, List[Int]) = this match {
    case QEmpty2 => throw new Exception
    case QNode2(front, back) => {
      if(back.isEmpty) {
        if(!front.isEmpty) {
          val obrni = front.reverse
          (QNode2(Nil, obrni.tail), obrni.head)
        } else {
          throw new Exception
        }
      } else (QNode2(front, back.tail),back.head)
    }
  }
}

case object QEmpty2 extends Q2
case class QNode2(front:List[List[Int]], back:List[List[Int]]) extends Q2

object Q2{
  def countAcc(ops: List[Option[List[Int]]]):Int = {
    def helper(acc:Int, leftSize:Int, rightSize:Int, left: List[Option[List[Int]]]):Int = left match {
      case Nil => acc
      case Some(x) :: tail => helper(acc+1, leftSize +1, rightSize, left.tail)
      case _ => 
              {
                if(rightSize == 0) helper(acc + leftSize + 1, 0, rightSize + leftSize, left.tail)
                else helper(acc+1, leftSize, rightSize-1, left.tail)
              }
    }
    helper(0,0,0,ops)

  }
}


// Naloga 2. preiskovanje labirinta

class Maze(nRows:Int, nCols:Int, cells:List[Boolean]){


  def vrniZnak(x:Int , y:Int):Boolean = cells(x + (y * nCols))



  def izpisi(x:Int, y:Int, left:List[Boolean], acc: String): String = {
    if(left.isEmpty) acc
    else {
      val dodaj = if(left.head == true) " " else "*"
      if(x == nCols-1) {
        izpisi(0, y+1, left.tail, acc + dodaj + "\n")
      } else {
        izpisi(x+1,y,left.tail, acc  + dodaj)
      }
    }
  }


  // ZMANJKALO CAJTA ZARADI KOLOKVIJA ARS


  def solution:List[Int] = {

    def najdiPot(x:Int, y:Int, pot:Q2, labirint:List[Boolean]):Boolean = {
      if(x == nCols -1 && y == nRows -1) true
      else if(vrniZnak(x,y) == false) false
      else {
        pot.enQ(List(x,y))
        println(List(x,y))
        println(izpisi(0,0, labirint, ""))

        val novi = labirint.updated((x+y*nCols), false)
        if(najdiPot(x+1, y, pot, novi)) true
        if(najdiPot(x, y-1, pot, novi)) true
        if(najdiPot(x-1, y, pot, novi)) true
        if(najdiPot(x, y+1, pot, novi)) true

        else false
      }
    }


    val resitev = najdiPot(0,1, QEmpty2, cells)
    Nil
  }
}

object Maze{
  def apply(m: String):Maze = ???
}
