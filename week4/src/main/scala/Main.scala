 trait BST {
   def isEmpty:Boolean
   def &(n:Int):BST
   def head:Int
   def left:BST
   def right:BST
   def size:Int
   def toList:List[Int]
   def apply(idx:Int):Int
   def toSortedList:List[Int]
   def merge(other:BST):BST
   def map(f:Int=>Int):BST
   def foldInOrder(startVal:Int)(f:(Int, Int)=>Int):Int
   def filter(f:Int=>Boolean):BST
   def equals(other:BST): Boolean
   def getCoords(depth: Int, offset: Int): List[(Int,Int,Int)]
 }

object EmptyT extends BST {
  def isEmpty:Boolean = true
  def &(n:Int):BST = new Node(n, EmptyT, EmptyT)
  def head:Int = throw new IllegalArgumentException
  def left:BST = throw new IllegalArgumentException
  def right:BST = throw new IllegalArgumentException
  def size:Int = 0
  def toList:List[Int] = Nil
  def map(f:Int=>Int):BST = EmptyT
  def apply(idx:Int):Int = throw new IndexOutOfBoundsException
  def toSortedList:List[Int] = Nil
  def merge(other:BST):BST = other
  def foldInOrder(startVal:Int)(f:(Int,Int)=> Int):Int = throw new IndexOutOfBoundsException
  def filter(f:Int=>Boolean):BST = EmptyT
  def equals(other:BST):Boolean = other.isEmpty
  def getCoords(depth: Int, offset: Int):List[(Int,Int,Int)] = Nil
  override def toString:String = "  EmptyT  "

}


class Node(element:Int, l:BST, r:BST) extends BST{
  def isEmpty:Boolean = false
  def &(n:Int):BST = {
    if(n <= element) {
      new Node(element, l & n, r)
    }
    else {
      new Node(element, l, r & n)
    }
  }
  def head:Int = element
  def left:BST = l
  def right:BST = r
  def size:Int = 1 + l.size + r.size
  def toList:List[Int] = element :: l.toList ::: r.toList
  def map(f:Int=>Int):BST = {
    def inner(acc: BST, opa: BST):BST ={
      if(opa.isEmpty) acc
      else inner(inner(acc & f(opa.head), opa.left),opa.right)
    }
    inner(EmptyT, this)
  }
  def toSortedList:List[Int] = l.toSortedList ::: List(head) ::: r.toSortedList
  def apply(idx:Int):Int = toSortedList(idx)
  def merge(other: BST):BST = other.toList.foldLeft(this:BST)((acc,x) => acc & x)
  def filter(f:Int=>Boolean):BST = toList.foldLeft(EmptyT:BST)((acc,x) => if(f(x)) acc & x else acc)
  def foldInOrder(startVal:Int)(f:(Int,Int)=> Int):Int = toList.foldLeft(startVal)(f)
  def equals(other:BST):Boolean = head == other.head && r.equals(other.right)  && l.equals(other.left)

  def getCoords(depth:Int, offset:Int): List[(Int,Int,Int)] = {
    // Na vaših testih funkcija ne bo delala, saj sem delam z malo drugačnimi offseti
    val leveKordinate = l.getCoords(depth + 1, offset)
    val leviOfsset = if (leveKordinate.length == 0)  offset - 2 else leveKordinate.map(x => x._1).max
    val c = (leviOfsset + 2, depth, head) :: leveKordinate
    c ::: r.getCoords(depth + 1, leviOfsset + 4)
  }

  override def toString:String = {
    // Tudi ta funkcija ne bo returnala isto kot vaša, saj so tudi kordinate drugačne. Ta izpis se mi je zdel lepši :)
    val kordinate = getCoords(0,0)
    // one liner :)
    "\n" + (0 until kordinate.map(x => x._2).max + 1) // za vsako vrstico
    .foldLeft("")(
      (acc,globina) => acc + kordinate.filter(x => x._2 == globina) // filtrira po globini
                                      .sortWith((x, y) => x._1 < y._1) // sortira po offsetu
                                      .foldLeft("")((acc, x) => acc + "." *( x._1 - acc.length) + x._3) + "\n") // zgradi vrstico
  }
}

//spremljevalni objekt razreda BST
object BST{
  def apply(l:Int*):BST = l.foldLeft(EmptyT: BST)((acc, x) => acc & x)

}
