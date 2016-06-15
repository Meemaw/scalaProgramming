//Scalacheck Naloga
import scala.math._

object ScalacheckEx{
  def encode(s:String, move:Int):String = s.map(x => (x.toInt + move).toChar)
  
  def decode(s:String, move:Int):String =  s.map(x => (x.toInt - move).toChar)

  def duplicate(l:List[Int]):List[Int] = l.foldLeft(l)((acc ,c) => acc.::(c))

  def isPalindrome(s:String):Boolean = {
    if(s.length == 1) true
    else if(s.head != s.reverse.head) false
    else isPalindrome(s.tail.init)
  }

  def sort(l:List[Int]):List[Int] = {
    if(l.isEmpty) Nil
    else iSort(l.head, sort(l.tail))
  }

  def iSort(x:Int, l:List[Int]):List[Int] = {
    if(l.isEmpty || x < l.head) x :: l
    else l.head :: iSort(x, l.tail)
  }
}




//Naloga 1.
trait Nat {

  def evaluate:Int = this match{
    case S(Z) => 0
    case S(x) => 1 + x.evaluate
  }

  def +(other:Nat):Nat = (this,other) match {
    case (x,y) => Nat(x.evaluate + y.evaluate)
  }

  def *(other:Nat):Nat = (this, other) match {
    case (x,y) => Nat(x.evaluate * y.evaluate)
  }

  def -(other:Nat):Nat = (this, other) match {
    case (x,y) => Nat(x.evaluate - y.evaluate)
  }

  def <(other:Nat):Boolean = (this,other) match {
    case (x,y) => x.evaluate < y.evaluate
  }
}

case class S(n:Nat) extends Nat
case object Z extends Nat


object Nat {
  def apply(n:Int):Nat = {
    if(n < 0) throw new Exception
    if(n == 0) S(Z)
    else S(apply(n-1))
  }

}



//Naloga 2.
trait Tree
class Node(left: Tree, right:Tree) extends Tree
object Empty extends Tree

object Tree {

  def genTrees(n:Int) : List[Tree] = ???
  


}


//Naloga 3.
case class C(real: Double, img:Double) {

  def +(other: C):C = (this,other) match {
    case (C(r,i),C(rr,ii)) => C(r+rr,i+ii)
  }

  def -(other: C):C = (this,other) match {
    case (C(r,i),C(rr,ii)) => C(r-rr,i-ii)
  }

  def *(other:C):C = (this, other) match {
    case (C(r,i), C(rr,ii)) => C(r*rr-i*ii, i*rr+r*ii)
  }

  def evaluate:Double = this match {
    case C(r,i) => Math.sqrt(r*r + i*i)
  }
}

object Mandelbrot{
  def cSeq(c:C):Stream[C] = C(0,0) #:: cSeq(c).map(x => (x*x)+c)

  def isDivergent(seq:Stream[C]):Boolean = seq.take(1000).exists(x => Math.abs(x.evaluate) > 2)

  val kordinate = for(y <- -1.0 to 1.0 by 0.1; x <- -2.0 until 1.0 by 0.05) yield (y,x)
  def draw:String = {
    def accomulate(acc:String, x:Int, y:Int, i:Int):String = {
      if(y == 20)  acc
      else {
        val t = kordinate(i)
        val str = if(isDivergent(cSeq(C(t._1,t._2)))) "*" else " "
        if(x == 59) accomulate(acc+str+"\n", 0,y+1,i+1)
        else accomulate(acc+str, x+1,y,i+1)
      }
    }
    accomulate("",0,0,0)
  }
}
