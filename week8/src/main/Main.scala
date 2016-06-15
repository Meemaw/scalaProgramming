
import scala.math._//Naloga 1.



object InfLists {

  def factorial(n:Int):Int = if (n==0) 1 else n * factorial(n-1)
  def from(n: Int): Stream[Int] = n#::from(n+1)
  val naturals: Stream[Int] = 1#::naturals.map(x=>x+1)
  val evens: Stream[Int] = 2#::evens.map(x=>x+2)
  val notEvens: Stream[Int] = 1#::notEvens.map(x=>x+2)
  val toTest:Stream[Double] = 1#::toTest.map(x=>x+1)



  val exCoef:Stream[Double] = 1#::naturals.map(t => (1.0 / factorial(t)))
  val cosCoef:Stream[Double] = 1#::naturals.map(x => if(x % 2 == 1) 0 else if(x % 4 == 0) 1.0/factorial(x) else -1.0/factorial(x))
  val sinCoef:Stream[Double] = 0#::naturals.map(x => if(x % 2 == 0) 0 else if((x+1) % 4 == 0) -1.0 / factorial(x) else 1.0 / factorial(x))

  val recaman:Stream[Int] = 1#::recaman.zip(naturals.drop(1)).map(t => t._1 + t._2 * (if(t._1 - t._2 > 0 && !recaman.take(t._2-1).toList.contains(t._1 - t._2)) -1 else 1))

}

//Naloga 2.
object Collatz {


  def zracunaj(n:Int, p:Int):Int = {
    if(p == 0) n/2
    else 3*n+1
  }


  def collatz(n:Int, acc:Stream[Int]):Stream[Int] = {
    if(n == 1) acc
    else if(n % 2 == 0) collatz(n/2, acc:+zracunaj(n,0))
    else collatz(3*n +1, acc:+zracunaj(n,1))
  }
  val naturals: Stream[Int] = 1#::naturals.map(x=>x+1)

  def trace(n: Int): Stream[Int] = n#::collatz(n,Stream())
  def allTraces: Stream[Stream[Int]] = naturals.map(x => trace(x))
  def findMaxSteps(n: Int): (Int, Int) = {
    def helper(c:Int, index:Int, max:Int):(Int,Int) = {
      if(c > n) (index, max)
      else {
        val current = allTraces(c-1).length
        if(current > max) helper(c+1, c, current)
        else helper(c+1,index,max)
      }
    }
    helper(1,0,0)
  }
}

//Naloga 3.
object PowerSeries {
  def apply(f: Int => Double): PowerSeries = new PowerSeries(InfLists.from(0).map(x => f(x)))
  val ex:PowerSeries = new PowerSeries(InfLists.exCoef)
  val cos:PowerSeries = new PowerSeries(InfLists.cosCoef)
  val sin:PowerSeries = new PowerSeries(InfLists.sinCoef)
}
case class PowerSeries(coef: Stream[Double]) {
  def from(n: Int): Stream[Double] = n#::from(n+1)


  def apply(x: Double): Stream[Double] = from(1).map(y => pomozna(x).take(y.toInt).foldLeft(0.0)((c,acc) => c + acc))

  def pomozna(x: Double): Stream[Double] = coef(0)#::coef.tail.zip(from(1)).map(t => t._1*(math.pow(x,t._2)))
  def +(other: PowerSeries): PowerSeries = other match {
    case PowerSeries(c) => new PowerSeries(coef.zip(c).map(t => t._1 + t._2))
    case _ => throw new Exception
  }
  def derive: PowerSeries = new PowerSeries(coef.zip(from(0)).map(t => t._1 * t._2).drop(1))
}
